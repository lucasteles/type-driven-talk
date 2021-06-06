module TypeDriven.Domain

open System

type NumeroCheque = NumeroCheque of int
type NumeroCartao = NumeroCartao of string

type TipoDeCartao = Visa | Mastercard

type CartaoDeCreditoInfo = TipoDeCartao * NumeroCartao

type MetodoDePagamento =
    | Dinheiro
    | Cheque of NumeroCheque
    | Cartao of CartaoDeCreditoInfo

[<Measure>] type Reais
type Valor = decimal<Reais>

type Moeda = BRL | USD

type Pagamento = {
    Valor: Valor
    Moeda: Moeda
    Metodo: MetodoDePagamento
}

type Conta = {
   Codigo: Guid
   ValorTotal: Valor
   ValorMinimo: Valor
   Vencimento: DateTime
   // outras informacoes como cliente. origem, etc
}

type ContaPaga = { CodigoDaConta:Guid; Data: DateTime }

// aqui ja é uma evolução do Processo

// vamos ter um evento sobre o sucesso da conta ou nao

// primeira iteração, só happy path
type PagarConta1 = Conta -> Pagamento -> ContaPaga

// mas e se eu tiver que validar a conta? como se ela estiver vencida, ou pagamento nao bater o valor minimo?
// talvez um boolean?
type PagarConta2 = Conta -> Pagamento -> (bool * ContaPaga)
// e sempre preciso validar o par de evento e booleano
// mas nao é muito descritivo

type PagarConta3 = Conta -> Pagamento -> ContaPaga option
// melhor ja que o option me fala se tem valor ou nao
// mas ainda nao temos como dizer o pq de ter falhado

// usamos uma forma mais inteligente
type PagarConta4 = Conta -> Pagamento -> Result<ContaPaga, string>
// esse me retorna uma string agora contendo o erro e consigo facilmente com um match saber se a conta deu certo
//ex

let pagarConta: PagarConta4 = failwith "nao implementado"

// exemplo de como isso ajuda a garantir o processamento
let processaEvento conta =
    let meio = {
        Valor = conta.ValorTotal
        Moeda = Moeda.BRL
        Metodo = MetodoDePagamento.Dinheiro
    }
    let tentaPagar = pagarConta conta meio
    match tentaPagar with
    | Ok evento -> printfn $"Conta {evento.CodigoDaConta} paga com sucesso"
    | Error erro -> printfn $"Deu ruim: {erro}"

// Porem strings sao pessimas,
// podemos modelar os erros em forma de Tipos tbm

type PagarContaErros =
    | ContaVencida of DateTime
    | ValorPagoMenorQueOValorMinimo
    | PagamentoAscimaDoValorDaConta

// assim temos uma assinatura que diz mais sobre o nosso modelo
type PagarConta5 = Conta -> Pagamento -> Result<ContaPaga, PagarContaErros>


// claro que os erros precisam virar string em algum momento, mas é trivial e seguro dessa forma
type ConverteErro = PagarContaErros -> string

//implementação
let obterTextoDoErro: ConverteErro =
    function
    | ContaVencida data -> $"A conta esta vencida desde {data}"
    | ValorPagoMenorQueOValorMinimo -> "O valor do pagamento nao é o minimo"
    | PagamentoAscimaDoValorDaConta -> "Pagamento excede o valor da conta"


// Daqui pra frente  é simples de evoluir nosso modelo com segurança
// a implementação estara muito mais guiada
// é uma documentação viva do dominio


// Mas dependencias?
// em FP é comum usarmos funcoes como dependencias
// imagine que precisemos validar se o pagameto pode ser realizado em outra api

// nao importa como validar, nosso fluxo so precisa saber de uma forma de consultar isso
// entao podemos adicionar apenas uma dependencia de uma funcao

type ValidaPagameto = Pagamento -> bool

// e adicionamos no nosso fluxo

type PagarConta = ValidaPagameto -> Conta -> Pagamento -> Result<ContaPaga, PagarContaErros>
// isso ja vai mudar a implementação
// deixa claro a dependencia do meu fluxo

//Claro que numa aplicação real teriamos que lidar com o resto da aplicação, entradas e saidas, dtos e http responses
// tudo isso pode seguir o mesmo modelo
type HttpResponse = { Body: obj; Status: int }

type ContaPagaRepostaHttp = Result<ContaPaga, PagarContaErros> -> HttpResponse

type PagamentoDTO = {
    Valor: decimal
    Moeda: string
    Metodo: string
    ContaCodigo: Guid
 }
type ObterContaPorId = Guid -> Conta

// por fim poderiamos ter a fluxo completo, combinando com outro fluxo e tendo uma request http de saida
type FluxoPagarConta =
        ObterContaPorId    // --| dependendencias
         -> ValidaPagameto // --|
         -> PagamentoDTO
         -> PagarConta
         -> HttpResponse

//<implementação fake nao vai ta no slide>
let dtoParaEntidade: PagamentoDTO -> Pagamento  = failwith "nao implementado"
//</implementação fake nao vai ta no slide>

// seria até automatico implementar esse codigo
let fluxoPagarConta: FluxoPagarConta =
    fun obterContaPorId validarConta pagamentoDto pagarConta ->

    let pagamento = dtoParaEntidade pagamentoDto
    let conta = obterContaPorId pagamentoDto.ContaCodigo
    let pagar = pagarConta validarConta

    let resultado = pagar conta pagamento
    match resultado with
    | Ok taPago -> {Body = taPago; Status=200}
    | Error erro -> {Body = obterTextoDoErro erro; Status=400}



