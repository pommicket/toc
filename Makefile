toc: *.[ch]
	./build.sh 
release: *.[ch]
	./build.sh release

clean:
	rm toc
