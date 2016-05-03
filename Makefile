build:
	stack build

debug: build
	stack exec sudo debug

ghci-fingerd:
	stack ghci --main-is fingerd:exe:fingerd

ghci-debug:
	stack ghci --main-is fingerd:exe:debug

run: build
	stack exec sudo fingerd

