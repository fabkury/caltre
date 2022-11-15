# caltre.R --------------------------------------------------------------------------------------------------------
# --
# Por Fabrício Kury, github at kury.dev
# 2021/5/16 11:39
# Coluna de margem a 120 caracteres.
# Afazeres:
#  - Implementar retorno de lista dos top-X melhores treinos, em vez de apenas o melhor. 
##
setwd(paste0(Sys.getenv("USERPROFILE"), '/OneDrive/Corpo/Força/caltre/'))
source(paste0(Sys.getenv("HOME"), '/Trabalho/comum/kury - espinha.R'))


# Controle --------------------------------------------------------------------------------------------------------
xlsx_arq <- '../Força.xlsx'
sinônimo <- read_xlsx(xlsx_arq, sheet = 'sinônimo')
conflito <- read_xlsx(xlsx_arq, sheet = 'conflito')
lim_dias_treino <- 60

# Faz
faz_anela <- FALSE

faz_exporta <- TRUE

faz_fórmula <- FALSE


# Anelamento simulado
anela_treino_entra <- 'demo'
entra_step <- 0.0004
voltas <- 5
entra_k_max <- 5000
def_report <- ceiling(entra_k_max/5)

# Exportação de treino em texto (ex.: para o Google Keep)
exporta_n_dia_início <- 1
exporta_data_início <- as.Date('2022-11-03') # NA
exporta_n_dias <- 8
exporta_treino_a <- 'demo'
exporta_dia_transição <- NA
exporta_treino_b <- 'base'


# Lê treino -------------------------------------------------------------------------------------------------------
lê_treino <- function(arq_entra, aba_entra) {
  treino <- read_xlsx(arq_entra, sheet = aba_entra) |>
    select(Sér., Freq., Def, `Grupo muscular`) |>
    filter(row_number() > 1) |>
    filter(!is.na(Sér.) & Sér. > 0)
  
  # Ajusta os tipos das colunas.
  treino %<>% mutate(
      Sér. = as.numeric(Sér.),
      Freq. = as.numeric(Freq.),
      Def = as.numeric(Def)) %>%
    mutate_all(~ifelse(is.na(.), 0, .))
  
  if(exists('fator_freq'))
    treino %<>% mutate(Freq. = round(Freq. * fator_freq))
  
  dias_treino <<- min(reduce(treino$Freq., lcm), lim_dias_treino)
  r_i <<- 1:dias_treino - 1

  treino %<>% faz_normaliza
  treino %<>% subdivide_pre
  
  treino
}

faz_normaliza <- function(treino, fator_red = 1) {
  # Normaliza grupos musculares, por ex. lombar demora mais que os outros.
  treino$Val <- treino$Sér.
  
  # lombar
  i <- which(treino$`Grupo muscular` == 'lombar')
  if(!is.null(i))
    treino$Val[i] <- ceiling(treino$Val[i] * 1.6)
  
  # coxa anterior
  i <- which(treino$`Grupo muscular` == 'coxa anterior')
  if(!is.null(i))
    treino$Val[i] <- ceiling(treino$Val[i] * 1.4)
  
  # trapézio
  i <- which(treino$`Grupo muscular` == 'trapézio')
  if(!is.null(i))
    treino$Val[i] <- ceiling(treino$Val[i] * 1.4)
  
  # Aplicar o fator de redução, caso exista na prática (i.e. seja diferente de 1).
  treino$Val <- treino$Val * fator_red
  
  treino
}

subdivide_pre <- function(treino) {
  nt <- treino %>%
    left_join(by = c('Grupo muscular' = 'origem'),
      filter(sinônimo, tipo == 'i')) %>%
    mutate(`Grupo muscular` = coalesce(inclui, `Grupo muscular`)) %>%
    select(-inclui)
  
  nt$Sér.[!is.na(nt$tipo) & nt$tipo == 'i' & !is.na(nt$Sér.)] <-
    nt$Sér.[!is.na(nt$tipo) & nt$tipo == 'i' & !is.na(nt$Sér.)] / 2
  nt$Val[!is.na(nt$tipo) & nt$tipo == 'i' & !is.na(nt$Val)] <-
    nt$Val[!is.na(nt$tipo) & nt$tipo == 'i' & !is.na(nt$Val)] / 2
  return(nt %>% select(-tipo))
}

subdivide_pós <- function(treino) {
  nt <- treino %>%
    left_join(by = c('Grupo muscular' = 'origem'),
      filter(sinônimo, tipo == 'f')) %>%
    mutate(`Grupo muscular` = coalesce(inclui, `Grupo muscular`)) %>%
    select(-inclui)
  
  másc <- !is.na(nt$tipo) & nt$tipo == 'f' & !is.na(nt$Freq.)
  
  nt %<>% select(-tipo)
  
  nt$Freq.[másc] <- nt$Freq.[másc] * 2
  
  # Pegar apenas os itens ímpares. ATENÇÃO: Estou assumindo que os itens associados (ex. abdominal
  # -> adominal inferior + abdominal superior) estão em linhas consecutivas no objeto nt/treino.
  másc <- másc & (1:length(másc))%%2
  nt$Def[másc] <- (nt$Def[másc] + nt$Freq.[másc]/2)%%nt$Freq.[másc]
  
  nt <- nt %>%
    left_join(by = c('Grupo muscular' = 'origem'),
      filter(sinônimo, tipo == 's')) %>%
    mutate(`Grupo muscular` = coalesce(inclui, `Grupo muscular`)) %>%
    select(-inclui)
  
  másc <- !is.na(nt$tipo) & nt$tipo == 's' & !is.na(nt$Sér.)
  nt %<>% select(-tipo)
  
  nt$Sér.[másc] <- nt$Sér.[másc] / 2
  
  nt
}


# Gera treino -----------------------------------------------------------------------------------------------------
viola_conflito <- function(d_new, bidirecional = TRUE) {
  li <- treino %>%
    mutate(Def = d_new) %>%
    select(Def, Freq., `Grupo muscular`)
  
  la <- li %>%
    inner_join(by = 'Grupo muscular',
      conflito %>%
        select(
          `Grupo muscular`,
          conflito
        ))
  
  lb <- li %>%
    inner_join(by = c('Grupo muscular' = 'conflito'),
      conflito %>%
        select(
          conflito#,
          # min_dias,
          # max_dias
        ))
  
  # TODO: Terminar de implementar min_dias e max_dias. 2022/8/17
  
  lc <- inner_join(la, lb, by = c('conflito' = 'Grupo muscular'))
  
    # return(any(lc$Def.y - lc$Def.x <= 1 & lc$Def.y - lc$Def.x >= -1)) # Verdadeiro se houver conflito.
  if(bidirecional) {
    return(any(lc$Def.y == lc$Def.x |
        lc$Def.y - lc$Def.x == 1 | lc$Def.y - lc$Def.x == -1%%lc$Freq..x |
        lc$Def.x - lc$Def.y == 1 | lc$Def.x - lc$Def.y == -1%%lc$Freq..x))
  } else {
    return(any(lc$Def.y == lc$Def.x |
        lc$Def.y - lc$Def.x == 1 | lc$Def.y - lc$Def.x == -1%%lc$Freq..x)) # Verdadeiro se houver conflito.
  }
}

vizinho <- function(d_vet, temp, max_violações = 100) {
  violações <- 0
  segue <- TRUE
  while(violações < max_violações && segue) {
    # Copia o treino.
    d_new <- d_vet
    
    # Mínimo é 1 dia alterado por vez, o máximo são todos menos 1.
    novos_dias <- 1 + round(runif(1)*(length(d_vet)-2)*temp)
    
    # Escolhe aleatoriamente quais dias serão alterados.
    másc <- sample(1:length(d_vet), novos_dias)
    
    # Gera valores aleatórios para os campos novos.
    d_new[másc] <- round(runif(length(másc))*(treino$Freq.[másc]-1))
    
    # Filtra valores proibidos.
    if(viola_conflito(d_new)) {
      violações <- violações + 1
    } else
      segue <- FALSE
      
    if(violações == max_violações) {
      message('!### ', max_violações, ' (máx.) violações atingidas.')
      segue <- FALSE
    }
  }
  
  return(d_new)
  
  # for(i in novos_masc)
  #   d_new[i] <- round(runif(1, min = 1, max = treino$Freq.[i]))
  # d_new
}


# Anelamento ------------------------------------------------------------------------------------------------------
anela <- function(p_fn, s0, k_max, step = entra_step, report = def_report, volta = 1) {
  s <- s0
  s_sd <- sd(calc_treino(s))
  
  s_min <- s
  s_min_sd <- s_sd
  # message('k_max = ', k_max)
  for(k in 1:k_max) {
    if(!(k%%report))
      message('k = ', k)
    
    temp <- (1 - step)^k
    # temp <- k/k_max #exp(-1 * k**(1/k_max))
    # temp <- 1/(1+exp(k-k_max/2))
    
    s_new <- vizinho(s, temp)
    
    s_new_sd <- p_fn(s_new)
    
    rg_res[k] <<- s_new_sd
    rg_temp[k] <<- temp
    rg_volta[k] <<- volta
    
    if(s_new_sd <= s_min_sd
      && any(s_new != s0)) {
      s_min <- s_new
      s_min_sd <- s_new_sd
    }
    
    # If P(E(s), E(snew), T) ≥ random(0, 1):
    dif <- s_new_sd - s_sd
    if(dif < 0 || runif(1) < exp(-dif/temp)) {
      s <- s_new
      s_sd <- s_new_sd
    }
  }
  
  # Output: the final state s
  s_min
}

lcm <- function(x, y) {
  # choose the greater number
  if(x > y) {
    greater = x
  } else {
    greater = y
  }
  while(TRUE) {
    if((greater %% x == 0) && (greater %% y == 0)) {
      lcm = greater
      break
    }
    greater = greater + 1
  }
  return(lcm)
}

calc_treino <- function(d_vet) {
  # d_vet: Vetor de defasagens, uma para cada linha do `treino`.
  
  ret_vet <- vector('integer', dias_treino)
  
  for(i in 1:nrow(treino)) {
    # Defasagem. 0 = nenhuma, 1 = 1 dia após, 2 = 2 dias após, etc.
    def <- d_vet[i]
    
    # Frequência. 1 = diária, 2 = a cada 2 dias, 3 = a cada 3 dias, etc.
    freq <- treino$Freq.[i]
    
    # Valor. Número de series por dia de treino, após ajustes da faz_normaliza().
    val <- treino$Val[i]
    
    # val_2 <- val * ((r_i - def) %% freq == 0)
    # val_2 <- cumsum(val_2) %% val
    # máscara <- ifelse(is.na(lag(val_2)), FALSE, val_2 < lag(val_2))
    
    # O treino incide nos dias ditos pela frequência, respeitando-se a defasagem.
    máscara <- (r_i - def)%%freq == 0
    
    # Adicionar o valor aos dias correspondentes.
    ret_vet[máscara] <- ret_vet[máscara] + val
  }
  
  ret_vet
}

calc_treino_antigo <- function(d_vet) {
  ret_vet <- vector('integer', dias_treino)
  for(i in 1:nrow(treino)) {
    def <- d_vet[i]
    freq <- treino$Freq.[i]
    val <- treino$Val[i]
    browser()
    ret_vet[(r_i - def)%%freq == 0] %<>% + val
  }
  ret_vet
}

calc_treino_sd <- function(d_vet) {
  sd(calc_treino(d_vet))
}

gráf_anelamento <- function() {
  plot.new()
  plot(list(x = 1:length(rg_res), y = unlist(rg_res)), type = 'p', pch = 20, cex = 0.25)
  lines(list(x = 1:length(rg_temp), y = unlist(rg_temp)))
}

gráf_tamanho_dia <- function() {
  calc_treino(treino$Def) %>% table %>% plot(type = 'l')
}

if(faz_anela) {
  # set.seed(42)
  console('Anelando, entra_k_max = ', entra_k_max, ', voltas = ', voltas, '.')
  
  if(!exists('treino')) {
    console('Lendo treino ', anela_treino_entra, ' do arquivo ', suppressWarnings(normalizePath(xlsx_arq)), '.')
    treino <- lê_treino(xlsx_arq, anela_treino_entra)
  }
  
  if(viola_conflito(treino$Def))
    message('!## Treino inicial viola conflito.')
  
  rg_res <- numeric(entra_k_max)
  rg_temp <- numeric(entra_k_max)
  rg_volta <- numeric(entra_k_max)

  for(loop in 1:voltas) {
    message('## Volta ', loop, ' de ', voltas, '.')
  
    # Opção 1: Gerar um treino aleatório.
    # s0 <- round(runif(length(treino$Freq.))*(treino$Freq.-1))
    
    # Opção 2: Usar o último treino
    s0 <- treino$Def
    
    message('Treino inicial:')
    print(treino)
    message('Dias: ', paste(calc_treino(s0), collapse = ' '))
    print(
      calc_treino(s0) |>
        table(dnn = 'Tabulação:')
    )
    
    message('Anelando a k_max = ', entra_k_max, ' e step = ', entra_step, '.')
    
    resval <- anela(calc_treino_sd, s0, entra_k_max, entra_step, volta = loop)
    
    if(calc_treino_sd(resval) <= calc_treino_sd(s0)
      && any(resval != s0)) {
      dnn_msg <- 'Anelamento completo. ' |>
        paste0(ifelse(calc_treino_sd(resval) == calc_treino_sd(s0),
          'Treino equivalente encontrado', 'Treino melhor encontrado'), ': ')
      treino <- treino |> mutate(Def = resval)
      print(treino)
      print(
        calc_treino(resval) |>
          table(dnn = dnn_msg)
      )
      message('Desvio-padrão: ', round(calc_treino_sd(resval), 3), '.')
    } else {
      message('Anelamento completo. Treino melhor não encontrado.')
      print(
        calc_treino(treino$Def) |>
          table(dnn = 'Tabulação atual:')
      )
    }
  }
  gráf_anelamento()
}


# Transição entre treinos -----------------------------------------------------------------------------------------
caltre <- function(dia_atual, treino_a,
  dia_trans = NA, # Dia da transição do treino A para o B.
  treino_b = NA) {
  # Estados:
  # 0: Não há transição, ou dia atual a precede.
  # 1: Há transição em curso.
  
  if(is.na(dia_trans) || dia_atual < dia_trans) {
    # Dia precede transição, ou não há transição.
    estado <- 0
    base <- gera_dia(dia_atual, treino_a)
    sobra <- base[0,]
    return(list(base = base, sobra = sobra, estado = estado, excesso = NA))
  }
  # Há uma transição em curso ou completa.
  estado <- 1
  
  # Queremos atingir a base B: dia atual no treino B.
  base_b <- gera_dia(dia_atual, treino_b)
  
  # O que temos disponível é a base A: todos os grupos musculares já trabalhados desde o início da transição.
  base_a <- dia_trans:dia_atual |>
    lapply(gera_dia, treino = treino_a) %>%
    bind_rows %>%
    distinct
  
  # Garantir presença de todos os grupos musculares na base_a.
  base_a %<>% union(
    treino_b |>
      select(`Grupo muscular`, Sér.) |>
      filter(!(`Grupo muscular` %in% treino_a$`Grupo muscular`)))
  
  # Coletar sobra caso o dia atual não seja o primeiro dia de transição.
  if(dia_atual > dia_trans) {
    # Recursar no dia aterior.
    tr_ant <- caltre(dia_atual-1, treino_a, dia_trans, treino_b)
    
    # O excesso é a sobra anterior, coletada hoje, que foi incluída hoje.
    excesso <- tr_ant$sobra |>
      filter(`Grupo muscular` %in% base_a$`Grupo muscular`
        | `Grupo muscular` %in% base_b$`Grupo muscular`)
    
    # A sobra passa a ser parte do objetivo, i.e. base B.
    base_b %<>% union(tr_ant$sobra)
  } else
    excesso <- NA
  
  # A base (dia) final é tudo aquilo na base B que constar na base A.
  base  <- base_b |>
    filter(`Grupo muscular` %in% base_a$`Grupo muscular`)
  
  # A sobra é o inverso.
  sobra <- base_b |>
    filter(! `Grupo muscular` %in% base_a$`Grupo muscular`)
  
  # Calcular estado da transição: já completa?
  if(all(treino_a$`Grupo muscular` %in% base_a$`Grupo muscular`)) {
    # Todos os grupos musculares já foram cobertos.
    # O dia atual é idêntico ao que seria no treino B.
    if(all(base$`Grupo muscular` %in% gera_dia(dia_atual, treino_b)$`Grupo muscular`))
      estado <- 2
  }
  
  return(list(base = base, sobra = sobra, estado = estado, excesso = excesso))
}


# Exportação ------------------------------------------------------------------------------------------------------
para_excel <- function(treino) {
  select(treino, -Val) %>%
    write.table('clipboard', sep = '\t', row.names = FALSE, col.names = FALSE)
}

imprime_rolo_de_treinos <- function(faixa_de_dias, treino_a, dia_trans = NA, treino_b = NA,
  primeiro_dia = exporta_data_início, nome_a = NA) {
  if(is.na(nome_a))
    nome_a <- deparse(substitute(treino_a))
  
  if(is.na(primeiro_dia))
    primeiro_dia <- lubridate::today()
  
  x <- lapply(faixa_de_dias, caltre, treino_a, dia_trans, treino_b)
  
  # Acrescentar coluna $Val
  x <- map(x, \(y) { map(y, \(z) {
    if(is.tbl(z))
      faz_normaliza(z)
    else
      z
    })
  })
  
  escreve_dia <- function(treino, percentil) {
    texto <- paste0('<', percentil, '>: \n',
      paste0(collapse = '\n', treino$`Grupo muscular`, ' ', treino$Sér.))
    texto
  }
  
  nível_dificuldade <- sapply(x, \(a) sum(a$base$Val))
  desnível <- max(nível_dificuldade)/2
  nível_dificuldade <- (nível_dificuldade-desnível)/max(nível_dificuldade)
  nível_dificuldade <- round(nível_dificuldade * 10)
  
  textos <- map(x, ~.$base) |> map2_chr(nível_dificuldade, escreve_dia)
  
  sobras <- map(x, ~sum(.$sobra$Sér.)) %>% unlist
  
  excessos <- map(x, ~sum(ifelse(is.na(.$excesso), 0, .$excesso$Sér.))) %>% unlist
  
  estados <- map(x, ~.$estado) %>% unlist

  datas <- seq(primeiro_dia, primeiro_dia + length(x) - 1, by = 'days') |> format('%m/%d')
  datas <- gsub('0(\\d)', '\\1', datas)
  
  complemento <- ifelse(is.na(dia_trans), '', deparse(substitute(treino_b)))
  
  título <- paste0(
    nome_a, ', ',
      '(', datas[1], ') ',
      faixa_de_dias[1], ' a ', faixa_de_dias[length(x)],
      ' (', datas[length(x)], ')',
    ifelse(is.na(dia_trans), '',
      paste0(' ', deparse(substitute(treino_b)), ' @ ', datas[dia_trans-min(faixa_de_dias)+1]))
    )
  
  gk <- tibble(
    dia = faixa_de_dias,
    data = datas,
    texto = textos,
    estado = estados,
    sobra = sobras,
    excesso = excessos
  ) %>%
    mutate(texto = case_when(
      estado == 0 ~ texto,
      estado == 1 ~ paste0('-', sobra, ifelse(excesso > 0, paste0(' +', excesso), ''), ' ', texto),
      TRUE ~ texto)) %>%
    mutate(texto = paste0('dia ', dia,
      # ' (', data, ') ', 
      ' ', texto))
  
  sai <- paste0(título, '\n\n',
    paste0(gk$texto, collapse = '\n\n'))
  
  sai
}

gera_dia <- function(d, treino) {
  másc <- (-treino$Def + d)%%treino$Freq. == 0
  treino[másc, c('Grupo muscular', 'Sér.')]
}

if(faz_exporta) {
  console('Exportando.')
  suppressPackageStartupMessages(library(clipr))
  
  # if(!exists('treino_a'))
  treino_a <- lê_treino(xlsx_arq, exporta_treino_a)
  if(!is.na(exporta_treino_b) && !is.na(exporta_dia_transição)) {
    # Há transição entre treinos.
    treino_b <- lê_treino(xlsx_arq, exporta_treino_b)
    res <- imprime_rolo_de_treinos(
      faixa_de_dias = exporta_n_dia_início:(exporta_n_dia_início+exporta_n_dias),
      treino_a = treino_a,
      treino_b = treino_b,
      dia_trans = exporta_dia_transição,
      nome_a = exporta_treino_a)
  } else {
    # Não há transição entre treinos.
    res <- imprime_rolo_de_treinos(
      faixa_de_dias = exporta_n_dia_início:(exporta_n_dia_início+exporta_n_dias),
      treino_a = treino_a,
      nome_a = exporta_treino_a)
  }
  
  write_clip(res, object_type = 'character')
  console(exporta_n_dias, ' dias de treino (',
    exporta_n_dia_início, '-', exporta_n_dia_início+exporta_n_dias,
    ') exportados para a área de transferência.')
}

