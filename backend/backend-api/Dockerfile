FROM haskell:9.10.3

WORKDIR /app

COPY backend/backend-api/stack.yaml ./stack.yaml
COPY backend/backend-api/backend-api.cabal ./backend-api.cabal

RUN stack setup

COPY backend/backend-api/ ./

RUN stack build

EXPOSE 10000

CMD ["stack", "run"]
