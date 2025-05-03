module Root.Exercicios.UnBCare where

import Root.Modelo.ModeloDados

{-
 

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo Modelo/ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med qtd [] = [(med, qtd)]
comprarMedicamento med qtd ((m, q):resto)
    | med == m   = (m, q + qtd) : resto
    | otherwise  = (m, q) : comprarMedicamento med qtd resto

{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento med estoque = case lookup med estoque of
   Just quantAtual -> if quantAtual > 0
      then Just $ (med, quantAtual - 1) : filter (\(m, _) -> m /= med) estoque
      else Nothing
   Nothing -> Nothing

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento med estoque = case lookup med estoque of
   Just quant -> quant
   Nothing -> 0

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos receituario = foldr (\(med, horarios) acc -> (med, length horarios) : acc) [] receituario


{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}
 -- | Verifica se uma lista tem todos os elementos distintos
todoDistintos :: Eq a => [a] -> Bool
todoDistintos [] = True
todoDistintos (x:xs) = x `notElem` xs && todoDistintos xs

-- | Verifica se uma lista está ordenada
estaOrdenada :: Ord a => [a] -> Bool
estaOrdenada [] = True
estaOrdenada [_] = True
estaOrdenada (x:y:xs) = x <= y && estaOrdenada (y:xs)

receituarioValido :: Receituario -> Bool
receituarioValido receituario =
  let medicamentos = map fst receituario
      medicamentosDistintos = todoDistintos medicamentos
      medicamentosOrdenados = estaOrdenada medicamentos
      horariosValidosPorMedicamento = 
        all (\(_, horarios) -> estaOrdenada horarios && todoDistintos horarios) receituario
  in medicamentosDistintos && medicamentosOrdenados && horariosValidosPorMedicamento

planoValido :: PlanoMedicamento -> Bool
planoValido plano =
  let horarios = map fst plano
      horariosDistintos = todoDistintos horarios
      horariosOrdenados = estaOrdenada horarios
      medicamentosValidosPorHorario = 
        all (\(_, medicamentos) -> todoDistintos medicamentos && estaOrdenada medicamentos) plano
  in horariosDistintos && horariosOrdenados && medicamentosValidosPorHorario

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}

-- Remove duplicatas de uma lista
removeDuplicatas :: Eq a => [a] -> [a]
removeDuplicatas [] = []
removeDuplicatas (x:xs) 
  | x `elem` xs = removeDuplicatas xs
  | otherwise = x : removeDuplicatas xs

-- Ordena uma lista (usando insertion sort)
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs) = inserir x (ordenar xs)
  where
    inserir y [] = [y]
    inserir y (z:zs)
      | y <= z = y : z : zs
      | otherwise = z : inserir y zs

plantaoValido :: Plantao -> Bool
plantaoValido plantao =
  let horarios = map fst plantao
      horariosDistintos = todoDistintos horarios
      horariosOrdenados = estaOrdenada horarios
      
      -- Verifica se não há compra e medicagem do mesmo medicamento no mesmo horário
      semCompraEMedicacaoSimultanea (_, cuidados) =
        let medicamentosComprados = [m | Comprar m _ <- cuidados]
            medicamentosMedicados = [m | Medicar m <- cuidados]
        in null [m | m <- medicamentosComprados, m `elem` medicamentosMedicados]
      
      -- Verifica se as medicações estão ordenadas lexicograficamente
      medicacoesOrdenadas (_, cuidados) =
        let medicacoes = [m | Medicar m <- cuidados]
        in estaOrdenada medicacoes
      
  in horariosDistintos && horariosOrdenados && 
     all semCompraEMedicacaoSimultanea plantao && 
     all medicacoesOrdenadas plantao

{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

-- | Gera um plano de medicamento a partir de um receituário válido
geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario receituario =
  let -- Expande o receituário para pares (horário, medicamento)
      expandeReceituario :: Receituario -> [(Horario, Medicamento)]
      expandeReceituario [] = []
      expandeReceituario ((med, horarios):resto) = 
        [(h, med) | h <- horarios] ++ expandeReceituario resto
      
      -- Agrupa por horário
      agrupaHorarios :: [(Horario, Medicamento)] -> [(Horario, [Medicamento])]
      agrupaHorarios [] = []
      agrupaHorarios pares =
        let
          -- Ordena pares por horário
          ordenaPares = ordenar pares
          
          -- Agrupa por horário
          agrupaPorHorario [] = []
          agrupaPorHorario ((h, m):resto) =
            let medicamentosNoHorario = m : [med | (hor, med) <- resto, hor == h]
                restoHorarios = [(hor, med) | (hor, med) <- resto, hor /= h]
                medicamentosOrdenados = ordenar (removeDuplicatas medicamentosNoHorario)
            in (h, medicamentosOrdenados) : agrupaPorHorario restoHorarios
        in agrupaPorHorario ordenaPares
      
      -- Ordena o plano final pelos horários
      ordenaPorHorario :: [(Horario, [Medicamento])] -> [(Horario, [Medicamento])]
      ordenaPorHorario = ordenar
      
      -- Todos os horários únicos no receituário, ordenados
      todosHorarios = ordenar $ removeDuplicatas $ concatMap snd receituario
      
      -- Todos os medicamentos para um dado horário
      medicamentosNoHorario h = [med | (med, hs) <- receituario, h `elem` hs]
      
      -- Monta o plano diretamente, mais eficiente
      montaPlano = [(h, ordenar $ removeDuplicatas $ medicamentosNoHorario h) | h <- todosHorarios]
  
  in montaPlano  -- Versão mais eficiente
  
{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

-- | Converte um plano de medicamentos para um receituário
-- | PlanoMedicamento: [(Horario, [Medicamento])] -> Receituario: [(Medicamento, [Horario])]
geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano plano =
    -- Agrupa os medicamentos e seus horários correspondentes
    let medicamentosHorarios = concatMap (\(h, meds) -> [(med, h) | med <- meds]) plano
        -- Agrupa os horários por medicamento
        agrupado = foldr (\(med, h) acc -> insertMedHorario med h acc) [] medicamentosHorarios
    in agrupado
  where
    -- Função auxiliar para inserir um horário na lista de horários de um medicamento
    insertMedHorario :: Medicamento -> Horario -> Receituario -> Receituario
    insertMedHorario med h [] = [(med, [h])]
    insertMedHorario med h ((m, hs):rest)
        | med == m  = (m, h:hs) : rest
        | otherwise = (m, hs) : insertMedHorario med h rest

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao [] estoque = Just estoque
executaPlantao ((_, []):resto) estoque = executaPlantao resto estoque
executaPlantao ((hora, (Comprar med qtd):cuidadosResto):plantaoResto) estoque =
  -- Executa a compra e continua com os cuidados restantes no mesmo horário
  let novoEstoque = comprarMedicamento med qtd estoque
  in executaPlantao ((hora, cuidadosResto):plantaoResto) novoEstoque
executaPlantao ((hora, (Medicar med):cuidadosResto):plantaoResto) estoque =
  -- Executa a medicação e continua com os cuidados restantes se possível
  case tomarMedicamento med estoque of
    Nothing -> Nothing  -- Falha se não houver medicamento suficiente
    Just novoEstoque -> executaPlantao ((hora, cuidadosResto):plantaoResto) novoEstoque
{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz plantao plano estoque =
  -- Verifica se o plantão pode ser executado completamente
  case executaPlantao plantao estoque of
    Nothing -> False  -- Se o plantão não pode ser executado, não satisfaz o plano
    Just _ -> 
      -- Verifica se todos os medicamentos prescritos no plano são ministrados no plantão
      all (\(horario, medicamentosPlano) -> 
        -- Para cada horário no plano, verifica se todos seus medicamentos são ministrados
        case lookup horario medicacoesPlantao of
          Nothing -> False  -- Se não há medicações neste horário no plantão
          Just medicamentosMinistrados -> 
            all (`elem` medicamentosMinistrados) medicamentosPlano
      ) plano
  where
    -- Cria um mapeamento de horários para medicamentos ministrados no plantão
    medicacoesPlantao = 
      [(h, [med | Medicar med <- cuidados]) | 
       (h, cuidados) <- plantao, 
       not (null [med | Medicar med <- cuidados])]

{-

QUESTÃO 11 VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto plano estoque =
  let
    -- Função que gera os cuidados para um horário específico
    gerarCuidadosHorario :: (Horario, [Medicamento]) -> EstoqueMedicamentos -> ([Cuidado], EstoqueMedicamentos)
    gerarCuidadosHorario (_, []) estq = ([], estq)  -- Sem medicamentos, sem cuidados
    gerarCuidadosHorario (h, (med:meds)) estq =
      let qtdDisponivel = consultarMedicamento med estq
          -- Se não há medicamento suficiente, precisa comprar
          (compras, estq') = if qtdDisponivel == 0
                             then ([Comprar med 1], comprarMedicamento med 1 estq)
                             else ([], estq)
          -- Medica o paciente com o medicamento atual
          estq'' = case tomarMedicamento med estq' of
                     Just novoEstq -> novoEstq
                     Nothing -> error "Impossível: acabou de comprar o medicamento"
          -- Gera cuidados para os medicamentos restantes
          (cuidadosResto, estqFinal) = gerarCuidadosHorario (h, meds) estq''
      in (compras ++ [Medicar med] ++ cuidadosResto, estqFinal)
    
    -- Gera o plantão a partir do plano, verificando o estoque
    gerarPlantao :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
    gerarPlantao [] _ = []
    gerarPlantao ((h, meds):resto) estq =
      let -- Ordena os medicamentos para garantir que o plantão seja válido
          medicamentosOrdenados = sort meds
          (cuidados, novoEstq) = gerarCuidadosHorario (h, medicamentosOrdenados) estq
      in (h, cuidados) : gerarPlantao resto novoEstq
    
    -- Função auxiliar para ordenar uma lista
    sort :: Ord a => [a] -> [a]
    sort [] = []
    sort (x:xs) = sort [y | y <- xs, y <= x] ++ [x] ++ sort [y | y <- xs, y > x]
    
  in gerarPlantao plano estoque