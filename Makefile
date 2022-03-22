build: 
	ghc -i ./src/Model/Grammar ./src/Parser/Grammar ./src/CnfRules ./src/NonSimpleRules ./src/Utils -o flp21-fun  --make ./src/Main.hs

test:
	ghc -i ./src/Model/Grammar ./src/Parser/Grammar ./src/CnfRules ./src/NonSimpleRules ./src/Utils -o flp21-fun  --make ./src/Main.hs
	sh ./test.sh

clean: 
	rm ./src/*.hi ./src/*.o flp21-fun


