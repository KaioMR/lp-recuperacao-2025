# Imagem base com GHC + Cabal
FROM haskell:9.10.3

# Instala curl (necessário para instalar o Stack)
RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*

# Instala o Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Diretório de trabalho dentro do container
WORKDIR /app

# Copia apenas os arquivos de config primeiro (melhor cache)
COPY backend/backend-api/stack.yaml backend/backend-api/backend-api.cabal ./

# Prepara o ambiente do Stack
RUN stack setup

# Agora copia o restante do código
COPY backend/backend-api/ ./

# Compila e instala o executável (vai para /root/.local/bin/backend-api)
RUN stack install

# A porta será informada pelo Render via variável PORT
# Opcional: expõe uma porta "default" (não precisa ser a real do Render)
EXPOSE 10000

# Comando de inicialização: roda o binário instalado pelo Stack
# Ele já lê a variável de ambiente PORT no seu Main.hs
CMD ["/root/.local/bin/backend-api"]
