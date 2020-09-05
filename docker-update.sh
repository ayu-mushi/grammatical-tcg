#/bin/bash
docker exec -it grammatical-tcg-1 cd grammatical-tcg; git pull
docker exec -it grammatical-tcg-1 cd grammatical-tcg; stack build & stack install
