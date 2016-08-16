#' Baixa um arquivo de audio mp3
#'
#' Recebe a mesma coisa que retorna e salva um
#' arquivo mp3 no caminho especificado
#'
#' @param arq character vector com um caminho para o arquivo. Se \code{NULL},
#' salva num arquivo temporario.
#'
#' @return nome do arquivo em que o mp3 foi salvo.
#'
#' @export
baixar <- function(arq = NULL) {
  r0 <- httr::GET('http://esaj.tjsc.jus.br/cposgtj/open.do')
  timestamp <- V8::v8()$eval("new Date().getTime();")
  r1 <- httr::POST('http://esaj.tjsc.jus.br/cposgtj/imagemCaptcha.do',
                   body = list('timestamp' = timestamp,
                               'uuidCaptcha' = '',
                               'conversationId' = ''))
  s <- jsonlite::fromJSON(httr::content(r1, 'text'))
  uid <- s$uuidCaptcha
  u <- sprintf('http://esaj.tjsc.jus.br/cposgtj/somCaptcha.do?timestamp=&uuidCaptcha=%s', uid)
  if (is.null(arq)) arq <- tempfile()
  httr::GET(u, httr::write_disk(arq, overwrite = TRUE))
  arq
}


#' Le e processa um arquivo mp3
#'
#' Le um arquivo mp3, converte para .wav com mpg123 (instale!), depois
#' usa o pacote \code{tuneR} para ler o arquivo. Pode ter vários side effects.
#'
#' @param a arquivo mp3 a ser lido
#' @param salvar se TRUE, salva arquivos .wav e .rds com os trechos do audio.
#' Se FALSE, não faz nada.
#' @param path pasta para salvar os arquivos caso salvar == TRUE.
#' @param deletar se TRUE, deleta o arquivo .wav gerado. Se FALSE, deixa
#' o arquivo na pasta getwd().
#'
#' @return um vetor de inteiros com a soma dos valores absolutos das frequencias
#' de cada trecho identificado. Esses numeros podem ser considerados como
#' uma fingerprint da letra, pois nenhuma outra letra possui a mesma soma
#' de frequencias.
#'
#' @export
ler <- function(a, salvar = FALSE, path = 'data-raw', deletar = TRUE) {
  a2 <- sprintf('./%s.wav', basename(a))
  # print(a2)
  system(sprintf('mpg123 -w %s %s', a2, a), ignore.stdout = TRUE,
         ignore.stderr = TRUE)
  x <- tuneR::readWave(a2)
  # print(x)
  x <- x@left
  if (deletar) file.remove(c(a, a2))
  colado <- paste(x, collapse = ',')
  spl_tot <- stringr::str_split(colado,  '(0,){50,}')[[1]]
  spl_num <- lapply(spl_tot, function(x) as.numeric(unlist(strsplit(x, ','))))
  spl <- sapply(spl_num, function(x) sum(x), USE.NAMES = FALSE)
  spl_num <- spl_num[-length(spl)]
  spl <- spl[-length(spl)]
  spl_num <- spl_num[-1]
  spl <- abs(spl[-1])
  spl_num <- spl_num[spl > 1000]
  spl <- spl[spl > 1000]
  if (salvar) {
    lapply(seq_along(spl), function(x) {
      w <- tuneR::Wave(left = spl_num[[x]], samp.rate = 22050, bit = 16)
      tuneR::writeWave(w, filename = sprintf('%s/%d.wav', path, spl[x]))
      saveRDS(spl_num[[x]], sprintf('%s/%d.rds', path, spl[x]))
    })
  }
  spl
}

#' Decifra o audio
#'
#' Le um arquivo de audio e tenta decifrar quais sao as letras.
#'
#' @param arq arquivo que contém o audio.
#' @param deletar se TRUE, deleta o arquivo .wav gerado. Se FALSE, deixa
#' o arquivo na pasta getwd().
#'
#' @return character vector de tamanho 1 com as letras preditas.
#'
#' @export
decifrar <- function(arq, deletar = TRUE) {
  # tab <- list(
  #   '114353' = 'x',
  #   '1499354' = 'i',
  #   '1612383' = 'b',
  #   '1789621' = 'u',
  #   '18366' = 's',
  #   '1989001' = 'c',
  #   '214136' = 'r',
  #   '2427812' = 'n',
  #   '2463118' = 'v',
  #   '2488708' = 'j',
  #   '3032330' = 'd',
  #   '3167291' = 'p',
  #   '4104273' = 'q',
  #   '4133498' = 'e',
  #   '515287' = 'z',
  #   '5702' = 'f',
  #   '5723' = 'k',
  #   '613236' = 'y',
  #   '624843' = 'h',
  #   '628017' = 'w',
  #   '637844' = 'a',
  #   '719394' = 'm',
  #   '820473' = 't'
  # )
  # tab <- dplyr::data_frame(soma = names(tab), letra = as.character(tab))
  s <- as.character(ler(arq, deletar = deletar))
  dd <- dplyr::data_frame(soma = s, ordem = 1:length(s))
  tab_f <- dplyr::inner_join(tab, dd, 'soma')
  letras <- dplyr::arrange(tab_f, ordem)$letra
  paste(letras, collapse = '')
}
