debug: *.[ch] build.sh
	./build.sh 
release: *.[ch] build.sh
	./build.sh release
profile: *.[ch] build.sh
	./build.sh profile
clean:
	rm toc *.o
