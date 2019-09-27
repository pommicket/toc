/* 
TODO:
casting
named return values
re-do cgen
 */
#include "toc.c"

int main(int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr, "Please specify an input file.\n");
		return EXIT_FAILURE;
	}

	const char *in_filename = argv[1];
	FILE *in = fopen(in_filename, "r");
	if (!in) {
		fprintf(stderr, "Could not open file: %s.\n", argv[1]);
		return EXIT_FAILURE;
	}
	
	char *contents = err_malloc(4096);
	long contents_cap = 4095;
	long contents_len = 0;
	while (fgets(contents + contents_len, (int)(contents_cap - contents_len), in)) {
		contents_len += (long)strlen(contents + contents_len);
		
		if (contents_len >= (long)contents_cap - 1024) {
			contents_cap *= 2;
			/* allocate +1 so that pointers don't overflow */
			contents = err_realloc(contents, (size_t)contents_cap + 1);
		}
	}
	if (ferror(in)) {
		fprintf(stderr, "Error reading input file: %s.\n", argv[1]);
		return EXIT_FAILURE;
	}
	fclose(in);

	err_filename = in_filename;
	Identifiers file_idents;
	idents_create(&file_idents);
	Tokenizer t;
	tokr_create(&t, &file_idents);
	if (!tokenize_string(&t, contents)) {
		err_fprint(TEXT_IMPORTANT("Errors occured while preprocessing.\n"));
		return EXIT_FAILURE;
	}
	
	arr_foreach(&t.tokens, Token, token) {
		if (token != t.tokens.data)
			printf("    ");
		fprint_token(stdout, token);
	}
	printf("\n");
	Parser p;
	parser_from_tokenizer(&p, &t);
   	ParsedFile f;
	if (!parse_file(&p, &f)) {
		err_fprint(TEXT_IMPORTANT("Errors occured while parsing.\n"));
		return EXIT_FAILURE;
	}

	
	block_enter(NULL, &f.stmts); /* enter global scope */
	if (!types_file(&f)) {
		err_fprint(TEXT_IMPORTANT("Errors occured while determining types.\n"));
		return EXIT_FAILURE;
	}
	
	fprint_parsed_file(stdout, &f);
	
	tokr_free(&t);

	
	
	/* TODO (eventually): use a tmp file (don't overwrite old output if there's an error) */
	/* const char *c_out_filename = "out.c"; */
	/* const char *h_out_filename = "out.h"; */
	/* FILE *c_out = fopen(c_out_filename, "w"); */
	/* FILE *h_out = fopen(h_out_filename, "w"); */
	/* CGenerator cgen; */
	/* cgen_create(&cgen, &file_idents, c_out, h_out, h_out_filename); */
	/* if (!cgen_file(&cgen, &f)) { */
	/* 	err_fprint(TEXT_IMPORTANT("Errors occured while generating C code.\n")); */
	/* 	return EXIT_FAILURE; */
	/* } */
	
	block_exit(NULL, &f.stmts); /* exit global scope */
	free(contents);
	
	/* fclose(c_out); */
	/* fclose(h_out); */
	idents_free(&file_idents);
}
