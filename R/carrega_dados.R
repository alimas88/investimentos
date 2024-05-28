# carrega e prepara os dados brutos
#' Carrega dados brutos
#'
#' @param dados_brutos Endereço com .xlsx orinal da B3
#'
#' @import readxl
#' @import dplyr
#' @import magrittr
#' @importFrom magrittr %>%
#'
#' @return df com operações - fracionário, campos data, e nomes colunas corrigidos
#' @export
#'
#' @examples df_dados_brutos <- carrega_dados('~/data/arquivo_dados.xlsx')
carrega_dados <- function(dados_brutos = '~\\sripts_r\\opcoes\\data\\negociacao-2022.xlsx') {


  dados_brutos <- readxl::read_xlsx(dados_brutos, sheet = 1) %>%

    # renomear colunas na forma tidy - poderia usar clean_names (janitor pack)
    renomeia_coluna() %>%
    select(!instituicao) %>%
    relocate(codigo_negociacao, .after = data_negocio) %>%
    relocate(c(quantidade, preco, valor), .after =  tipo_movimentacao) %>%

    # retira o 'F' das compras fracionárias
    mutate(codigo_negociacao = corrige_fracionario(codigo_negociacao)) %>%

    #data caractere para formato data. Quiet silencia warnings (NAs nas coluna data)
    mutate(data_negocio = lubridate::dmy(data_negocio),
           prazo_vencimento = lubridate::dmy(prazo_vencimento, quiet = TRUE)) %>%

    # dplyr atualizou. Ao invés de group_by, usar o parâmetro ".by ="
    summarise(quantidade = sum(quantidade), valor = sum(valor), preco = valor/quantidade,
              .by = c(data_negocio, codigo_negociacao, tipo_movimentacao, mercado, prazo_vencimento))

}



################__________________#################

#' Renomeia nomes das colunas
#'
#' @param df Data frame com colunas originais
#'
#' @importFrom dplyr rename
#'
#' @return df_nome_col
#' @export
#'
#' @examples df_renomeado <- renomeia_coluna(df)
renomeia_coluna <- function(df){

  df_nome_col <- df %>%
    rename(data_negocio = 'Data do Negócio', codigo_negociacao = "Código de Negociação",
           quantidade = "Quantidade", preco = "Preço", valor = "Valor",
           tipo_movimentacao = "Tipo de Movimentação",
           mercado = "Mercado", prazo_vencimento = "Prazo/Vencimento",
           instituicao = "Instituição")


}



#' Corrige campo codigo_negociacao
#'
#' @param codigo_negociacao A vector
#'
#' @importFrom stringr str_remove
#'
#' @return codigo_negociacao
#' @export
#'
#' @examples mutate(codigo_negociacao = corrige_fracionario(codigo_negociacao))
corrige_fracionario <- function(codigo_negociacao){

  codigo_negociacao <- str_remove(codigo_negociacao, "F$")

}
