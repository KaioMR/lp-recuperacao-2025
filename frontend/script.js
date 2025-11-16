// URL da API em Haskell (ajuste quando for fazer deploy)
const API_URL = "http://localhost:8080/api/compound";

const form = document.getElementById("compound-form");
const resultDiv = document.getElementById("result");
const errorDiv = document.getElementById("error");

form.addEventListener("submit", async function (event) {
  event.preventDefault(); // evita recarregar a página

  // Limpa mensagens anteriores
  resultDiv.textContent = "";
  errorDiv.textContent = "";

  // Lê valores do formulário
  const principalInput = document.getElementById("principal");
  const rateInput = document.getElementById("rate");
  const timesPerYearInput = document.getElementById("timesPerYear");
  const yearsInput = document.getElementById("years");

  const principal = parseFloat(principalInput.value);
  const ratePercent = parseFloat(rateInput.value);
  const timesPerYear = parseInt(timesPerYearInput.value, 10);
  const years = parseFloat(yearsInput.value);

  // Validação simples no frontend (além da do backend)
  if (isNaN(principal) || principal <= 0) {
    errorDiv.textContent = "Informe um valor inicial (P) maior que 0.";
    return;
  }

  if (isNaN(ratePercent) || ratePercent < 0) {
    errorDiv.textContent = "Informe uma taxa anual válida (em %).";
    return;
  }

  if (isNaN(timesPerYear) || timesPerYear < 1) {
    errorDiv.textContent =
      "Informe o número de capitalizações por ano (n) maior ou igual a 1.";
    return;
  }

  if (isNaN(years) || years <= 0) {
    errorDiv.textContent = "Informe o tempo em anos (t) maior que 0.";
    return;
  }

  // Converte taxa percentual para decimal (12% -> 0.12)
  const rate = ratePercent / 100;

  const body = {
    principal: principal,
    rate: rate,
    timesPerYear: timesPerYear,
    years: years,
  };

  try {
    const response = await fetch(API_URL, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(body),
    });

    const data = await response.json();

    if (!response.ok) {
      // Backend mandou erro (400 etc.)
      const message =
        data && data.message
          ? data.message
          : "Erro ao calcular os juros compostos.";
      errorDiv.textContent = message;
      return;
    }

    // Sucesso – mostra o valor final formatado
    const amount = data.amount;
    const formatted = amount.toFixed(2);

    resultDiv.textContent = `Montante final (A): R$ ${formatted}`;
  } catch (error) {
    console.error(error);
    errorDiv.textContent =
      "Não foi possível se conectar à API. Verifique se o backend está rodando.";
  }
});
