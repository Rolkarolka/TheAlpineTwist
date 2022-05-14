build_l: *.hs
	ghc main.hs -O2 -o build/main
	rm *.hi *.o

run: build_l
	build/main

build_w: *.hs
	ghc ./main.hs -O2 -o ./build/main.exe
	del *.hi *.o

run_w: build_w
	./build/main.exe

clean:
	rm *.hi *.o build/*

clean_w:
	del *.hi *.o ./build/*