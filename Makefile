toc: *.[ch] build.sh
	./build.sh 
release: *.[ch] build.sh
	./build.sh release

clean:
	rm toc *.o
