# Imagem base com GHC + Stack já instalados
FROM haskell:9.10.3

# Diretório de trabalho dentro do container
WORKDIR /app

# Copia arquivos de configuração do Stack e do Cabal
# (do repo -> para dentro de /app)
COPY backend/backend-api/stack.yaml ./stack.yaml
COPY backend/backend-api/backend-api.cabal ./backend-api.cabal

# Prepara o ambiente (baixa GHC, etc.)
RUN stack setup

# Agora copia o restante do código fonte
COPY backend/backend-api/ ./

# Compila e instala o executável
RUN stack install

# Porta interna do container (Render vai injetar PORT)
EXPOSE 10000

# Comando de inicialização:
# o binário instalado pelo stack costuma
