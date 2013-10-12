library(stringr)

match <- function(match, rest) {
  structure(list(match=match, rest=rest), class="match")
}

literal <- function(token) {
  l <- str_length(token)

  function(s) {
    ss <- str_sub(s, 1, l)

    if (ss == token) {
      return(match(ss, str_sub(s, l+1)))
    }
    
    return(NULL)
  }
}

`%-+-%` <- function(p1, p2) {
  p1 <- match.fun(p1)
  p2 <- match.fun(p2)
  
  function(s) {
    m1 <- p1(s)

    if (is.null(m1)) {
      return(NULL)
    }

    m2 <- p2(m1$rest)

    if (is.null(m2)) {
      return(NULL)
    }

    return(match(list(m1$match, m2$match), m2$rest))
  }
}

`%-|-%` <- function(p1, p2) {
  p1 <- match.fun(p1)
  p2 <- match.fun(p2)
  
  function(s) {
    m1 <- p1(s)

    if (!is.null(m1)) {
      return(m1)
    }

    m2 <- p2(s)

    if (!is.null(m2)) {
      return(m2)
    }

    return(NULL)
  }
}

