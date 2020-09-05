FROM haskell:8.8.3

WORKDIR /work

COPY stack.yaml package.yaml /work/
RUN stack build --only-dependencies

COPY . /work/
RUN stack build && stack install

CMD grammatical-tcg-hs-exe -p $PORT
