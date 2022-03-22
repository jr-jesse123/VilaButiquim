

// ==============================================
// implementação 
// =============================================


let localizarFuncionarioPorCpf funcionarios cpf= 
    funcionarios |> List.tryFind (fun (x:Controlefuncionario) -> x.Identificacao.CPF = cpf)


let RegistrarQuebracaixa : ``Registrar quebra de Caixa`` =
    fun (dia, valor) funcionario  -> 
        {funcionario with Descontos = QuebraDeCaixa (dia,valor) :: funcionario.Descontos }


let RegistrarFalta : ``Registrar Falta`` =
//TODO: IMPEDIR FALTA PARA DIA FUTURO E RESTRINGIR PARA PERÍODO CORRENTE
    fun  data funcionario -> 
        if funcionario.Descontos |> List.contains (Falta data) then failwith "não podem haver duas faltas no mesmo dia"
        {funcionario with Descontos = Falta data :: funcionario.Descontos |> List.distinct}

let InformarAdiantamento : ``Informar adiantamento de Salario`` =
    fun  (valor, data) funcionario -> 
        {funcionario with Descontos= AdiantamentoSalario (valor,data) :: funcionario.Descontos}


type ``Alterar Registro de Funcionario`` = Controlefuncionario list -> Controlefuncionario -> Controlefuncionario list
let adicionarFuncionario : ``Alterar Registro de Funcionario`` =
    fun funcionarios nvRegistro ->
        match localizarFuncionarioPorCpf funcionarios nvRegistro.Identificacao.CPF with
        |Some _ -> failwith "Já Existe um funcionáriuo com este cpf"
        |None -> nvRegistro :: funcionarios
        
let flip f a b = f b a 

let AtualizarRegistroFuncionario funcionarios nvRegistro = 
    match localizarFuncionarioPorCpf funcionarios nvRegistro.Identificacao.CPF with
    | Some registroAtual -> 
        funcionarios
        |> List.except [registroAtual]  
        |>  fun listaFuncionarios -> adicionarFuncionario listaFuncionarios nvRegistro

    | None -> failwith "Funcionário não encontrado"



// =====================================================
// DEMO
// =====================================================

let mutable RepositorioFuncionarios : Controlefuncionario list = []

// -----------------------------------------------
// Adicionar funcionarios 
// ---------------------------------------------

let hoje = DateOnly.FromDateTime DateTime.Today

let identificacao1 : DadosIdentificacao = {
    Nome = "Jessé"  
    CPF = "01724125109" 
    Funcao = "programador" 
    DataAdmissao = hoje
    Lotacao = "Loja 1" 
    DadosPagamento = Pix "01724125109"
}

let faturamento1 : CadastroFaturamento = {
    SalarioFormal = 1000M
    Impostos = ["IR", Porcentagem 0.2M ; "VA",  ValorFixo 10M ]
}


let funcionario1 : Controlefuncionario = {
    Identificacao = identificacao1
    Faturamento = faturamento1
    Descontos = []
    Creditos = []
}



//Funcionario1 adicionado ao repositorio 
RepositorioFuncionarios <-  adicionarFuncionario RepositorioFuncionarios funcionario1

//Outro funcionario adicioando ao repositorio
RepositorioFuncionarios <- adicionarFuncionario RepositorioFuncionarios {funcionario1 with Identificacao = {identificacao1 with CPF = "123456879"}}

// adicionar mesmo outro funcionario com o mesmo cpf nãoé possível
//RepositorioFuncionarios <-  adicionarFuncionario RepositorioFuncionarios funcionario1


// Localizando informações de um funcionário por CPF 
localizarFuncionarioPorCpf RepositorioFuncionarios "01724125109"


// Atualiza registro funcionario
let AtualizarRegistroFuncionario' funcionario = 
    RepositorioFuncionarios <- AtualizarRegistroFuncionario RepositorioFuncionarios funcionario 



// Registrar quebra de caixa
let RegistrarQuebracaixa' cpf (data, vlr)  = 
    localizarFuncionarioPorCpf RepositorioFuncionarios cpf
    |> Option.map (RegistrarQuebracaixa (data, vlr))
    |> Option.map AtualizarRegistroFuncionario'

RegistrarQuebracaixa'  "01724125109"  (hoje, 15M)

RepositorioFuncionarios


//registrar falta 
let RegistrarFalta' cpf data  = 
    //TODO: EXPERIMENTAR O RESPOSITÓRIO ACTION PARA ABSTRAIR O ACESSO À DADOS
    //TODO: EXPERIMENTAR MONADA COM ACESSO AO REPOSITÓRIO
    localizarFuncionarioPorCpf RepositorioFuncionarios cpf
    |> Option.map (RegistrarFalta data)
    |> Option.map AtualizarRegistroFuncionario'




RegistrarFalta'  "01724125109" (hoje.AddDays(-1))



let InformarAdiantamento' cpf (valor, data)  =
    localizarFuncionarioPorCpf RepositorioFuncionarios cpf
    |> Option.map (InformarAdiantamento (valor, data) )
    |> Option.map AtualizarRegistroFuncionario'


InformarAdiantamento' "01724125109" (10M,hoje) 
