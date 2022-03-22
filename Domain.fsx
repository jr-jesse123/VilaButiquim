open System

type Texto = string
type PorDefinir = unit

type Funcao = Texto

type Data = System.DateOnly
type Lotacao = Texto


type DadosBancarios = {
    Banco: Texto
    //Agencia: Texto
    Operacao: Texto
    Conta: Texto
}

type DadosPagamento = 
    | Transferecina  of DadosBancarios
    | Pix of ChavePix:Texto

type CPF = Texto


type DadosIdentificacao = {
    Nome : Texto
    CPF: CPF
    Funcao : Funcao
    DataAdmissao : Data
    Lotacao : Lotacao
    DadosPagamento : DadosPagamento
}

type Reais = decimal

type Porcentagem = decimal

type TipoImposto =
| ValorFixo of Reais 
| Porcentagem of Porcentagem

type NomeImposto = Texto

type Imposto = NomeImposto * TipoImposto


type CadastroFaturamento = {
    SalarioFormal : Reais
    Impostos : Imposto list
}
    
type ObterSalarioLiquido = CadastroFaturamento -> Reais



type ConvenioConsumido = {
    Dia:Data 
    Produtos: Texto list 
    Valor:Reais
}

type Desconto =
    |QuebraDeCaixa of Dia:Data * Valor:Reais
    |Falta of Data
    |AdiantamentoSalario of Valor:Reais * Dia:Data
    |Convenio of ConvenioConsumido
    |DescontoDiverso of   Valor:Reais * Observacoes:Texto




type Credito =
    |CreditoDiverso of  Valor:Reais * Observacoes:Texto

type Controlefuncionario = {
    Identificacao : DadosIdentificacao
    Faturamento: CadastroFaturamento
    Descontos : Desconto list
    Creditos : Credito list
}

type ``Calcular valor à receber`` = Controlefuncionario -> Reais

type ``Registrar quebra de Caixa`` =  Data * Reais -> Controlefuncionario -> Controlefuncionario

type ``Registrar Falta`` = Data ->  Controlefuncionario -> Controlefuncionario

type ``Informar adiantamento de Salario`` = Reais * Data -> Controlefuncionario ->    Controlefuncionario


type ``Registar uso convenio`` = Controlefuncionario -> ConvenioConsumido -> Controlefuncionario

let RegistrarUsoConvenio : ``Registar uso convenio`` =
    fun funcionario consumo ->      
        {funcionario with Descontos = Convenio consumo :: funcionario.Descontos}

type ``Descontar Funcionario`` = Controlefuncionario ->   Reais * Texto -> Controlefuncionario

let DescontarFuncionario : ``Descontar Funcionario`` =
    fun funcionario (valor,obs) -> 
        {funcionario with Descontos = DescontoDiverso (valor,obs) :: funcionario.Descontos}


type ``Creditar Funcionario`` = Controlefuncionario ->    Reais * Texto -> Controlefuncionario

let CreaditarFuncionario :``Creditar Funcionario`` =
    fun funcionario (valor,obs) ->  
        {funcionario with Creditos = CreditoDiverso (valor,obs) :: funcionario.Creditos}


type ``Obter Funcionario`` = Controlefuncionario list -> CPF -> Controlefuncionario
let ObterFuncionario : ``Obter Funcionario`` = 
    fun funcionarios cpf -> 
        funcionarios |> List.filter (fun x -> x.Identificacao.CPF = cpf) |> List.head

