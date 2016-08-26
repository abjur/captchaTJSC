#' @export
download_new <- function() {
  u <- 'https://esaj.tjsc.jus.br/cpopg/imagemCaptcha.do'
  r <- httr::GET(u)
  dados <- httr::content(r)
  dados
}

#' @export
ler_new <- function(dados) {
  img <- dados$imagem %>%
    strsplit(',') %>%
    dplyr::first() %>%
    dplyr::last() %>%
    base64enc::base64decode() %>%
    png::readPNG()
  img_dim <- dim(img)
  img_df <- data.frame(
    x = rep(1:img_dim[2], each = img_dim[1]),
    y = rep(img_dim[1]:1, img_dim[2]),
    r = as.vector(img[,,1]),
    g = as.vector(img[,,2]),
    b = as.vector(img[,,3])
  )
  color <- dados$labelValorCaptcha %>%
    xml2::read_html() %>%
    rvest::html_node('b') %>%
    rvest::html_text() %>%
    tolower()
  d <- dplyr::mutate(img_df, cor = rgb(r, g, b), id = 1:n())
  d <- dplyr::filter(d, cor != '#FFFFFF')
  d <- tibble::as_data_frame(d)
  d %>%
    dplyr::mutate(uuid = dados$uuidCaptcha) %>%
    dplyr::mutate(captcha_color = color)
}

#' @export
limpar_new <- function(aff) {
  if (nrow(aff) > 10000) {
    aff <- aff %>%
      dplyr::group_by(cor) %>%
      dplyr::mutate(n = n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n < max(n))
  }
  aff <- aff %>%
    dplyr::filter(y > 15, y <= 45) %>%
    dplyr::group_by(cor) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(rank = dplyr::dense_rank(n),
                  rank = max(rank) - rank) %>%
    dplyr::filter(rank %in% c(0, 1))

  cor <- aff$captcha_color[1]

  aff <- aff %>%
    dplyr::group_by(cor) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::ungroup()

  if (cor == 'azul') {
    aff <- aff %>%
      dplyr::filter((b - r - g) == max(b - r - g))
  } else if (cor == 'laranja') {
    aff <- aff %>%
      dplyr::filter((r + g) == max(r + g))
  } else if (cor == 'preto') {
    aff <- aff %>%
      dplyr::filter((r + g + b) == min(r + g + b))
  } else if (cor == 'rosa') {
    aff <- aff %>%
      dplyr::filter((r - g - b) == max(r - g - b))
  } else if (cor == 'roxo') {
    aff <- aff %>%
      dplyr::filter((r + b) == max(r + b))
  } else if (cor == 'verde') {
    aff <- aff %>%
      dplyr::filter((g - r - b) == max(g - r - b))
  } else if (cor == 'vermelho') {
    aff <- aff %>%
      dplyr::filter((r - g - b) == max(r - g - b))
  }
  aff
}

#' @export
ocrTesseract <- function (dir_path, image_name, output_base, lang = "eng", psm = 5) {
  command <- paste0("tesseract ", dir_path, "/", image_name,
                    " ", dir_path, "/", output_base,
                    " -l ", lang, " -psm ", psm,
                    ' outputbase text')
  system(command, wait = TRUE, ignore.stdout = T, ignore.stderr = T)
  result <- readLines(paste0(dir_path, "/", output_base, ".txt"))
  result
}

#' Instalar ocR
#'
#' https://github.com/greenore/ocR
#'
#'
#' criar o arquivo
#' /usr/share/tesseract-ocr/tessdata/configs
#' e adicionar o seguinte conteudo
#' tessedit_char_whitelist abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ
#'
#' @export
ocr <- function(dados) {
  rx <- range(dados$x)
  ry <- range(dados$y)
  tab_completa <- expand.grid(x = rx[1]:rx[2], y = ry[1]:ry[2])
  dados %>%
    dplyr::mutate(r = 0, b = 0, g = 0) %>%
    dplyr::right_join(tab_completa, c('x', 'y')) %>%
    dplyr::mutate(r = ifelse(is.na(r), 1, 0)) %>%
    converter_em_matriz() %>% {
      m <- .
      m[nrow(m):1, ]
    } %>%
    png::writePNG('aff.png')
  txt <- ocrTesseract('./', 'aff.png', 'aff', psm = 6) %>%
    paste(collapse = '') %>%
    stringr::str_trim() %>%
    stringr::str_replace_all('[^a-zA-Z]', '')
  file.remove('aff.png')
  file.remove('aff.txt')
  txt
}
