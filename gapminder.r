#####################################################################@@#
# Gapminder 
#
# Tokrat boste v R-ju poskušali napisati čimboljši približek orodja
# [Gapminder](http://www.gapminder.org/), ki smo si ga ogledali na
# prvih vajah.
#####################################################################@@#



################################################################@000287#
# 1) Sestavite funkcijo uvozi(datoteka), ki dano datoteko s podatki uvozi
# v razpredelnico. Funkcija naj ustrezno uvaža datoteke, ki so na voljo
# na [Gapminderju](http://www.gapminder.org/data/).
# Če želite CSV datoteke, skopirajte povezavo da XLS datotek ter na
# koncu &output=xls popravite v &output=csv.
# 
# Primeri datotek:
# 
# - [pričakovana življenska doba](http://spreadsheets.google.com/pub?key=phAwcNAVuyj2tPLxKvvnNPA&output=csv)
# - [GDP](http://spreadsheets.google.com/pub?key=phAwcNAVuyj1jiMAkmq1iMg&output=csv)
# - [velikost prebivalstva](http://spreadsheets.google.com/pub?key=phAwcNAVuyj0XOoBL_n5tAQ&output=csv)
################################################################000287@#

uvozi <- function(datoteka){
  tabela <- read.table(datoteka, header = TRUE, sep = ",", 
                       fill = TRUE, as.is = TRUE, 
                       row.names = 1, quote = "\"")
  države <- rownames(tabela)
  
  zamenjava <- function(vektor){
    vektor <- as.numeric(gsub(",", "", vektor))
    return(vektor)}
  
  tabela <- apply(tabela, 2, zamenjava)
  rownames(tabela) <- države
  return(tabela)}

################################################################@000288#
# 2) Sestavite funkcijo dopolni(tabela), ki vrednosti NA v tabeli
# nadomesti s približki, ki jih dobi iz sosednjih vrednosti v tabeli.
# 
# Namig: approx.
################################################################000288@#

# dopolni <- function(tabela){
#   približki <- approx(x, y, n = lenght(tabela))
#   tabela[tabela == NA] <- 
#   return(tabela)}


################################################################@000289#
# 3) Sestavite funkcijo prikazi(x, y, r, leto), ki sprejme razpredelnice
# x, y in r ter s krogi prikaže porazdelitev podatkov v danem
# letu.
# Razpredelnici x in y bosta predstavljali podatke, ki jih želimo
# primerjati, razpredelnica r pa bo običajno velikost prebivalstva.
# Pazite, da bo vrednost r premo-sorazmerna površini krogov.
# 
# Namig: symbols
################################################################000289@#



################################################################@000290#
# 4) Sestavite funkcijo lepo.prikazi(...), ki ponuja čim boljši
# uporabniški vmesnik do čim lepše predstavitve podatkov.
# Poskusite narediti logaritemske lestvice, barvanje, izbiranje držav,
# povečavo okna, in tako naprej.
# 
# Namig: manipulate (le v RStudiu), locator, …
################################################################000290@#











































































































#####################################################################@@#
# Kode pod to črto nikakor ne spreminjajte.
########################################################################

"TA VRSTICA JE PRAVILNA."
"ČE VAM R SPOROČI, DA JE V NJEJ NAPAKA, SE MOTI."
"NAPAKA JE NAJVERJETNEJE V ZADNJI VRSTICI VAŠE KODE."
"ČE JE NE NAJDETE, VPRAŠAJTE ASISTENTA."




























































get_current_filename <- function () {
  if (length(showConnections()) > 1) {
    return(showConnections()[1, "description"])
  } else {
    return(Find(Negate(is.null), Map(function(f) { f$ofile }, sys.frames()), right=TRUE))
  }
}
.filename <- get_current_filename()

.check <- function() {
  # This is the main file of the rjson package, available at
# http://cran.r-project.org/web/packages/rjson/
# We include it directly into our file because we prefer to have a single file
# and since rjson is not a standard R library and as such not available on all
# computers.

toJSON <- function( x )
{
    #convert factors to characters
    if( is.factor( x ) == TRUE ) {
        tmp_names <- names( x )
        x = as.character( x )
        names( x ) <- tmp_names
    }

    if( !is.vector(x) && !is.null(x) && !is.list(x) ) {
        x <- as.list( x )
        warning("JSON only supports vectors and lists - But I'll try anyways")
    }

    if( is.null(x) )
        return( "null" )

    #treat named vectors as lists
    if( is.null( names( x ) ) == FALSE ) {
        x <- as.list( x )
    }

    #named lists only
    if( is.list(x) && !is.null(names(x)) ) {
        if( any(duplicated(names(x))) )
            stop( "A JSON list must have unique names" );
        str = "{"
        first_elem = TRUE
        for( n in names(x) ) {
            if( first_elem )
                first_elem = FALSE
            else
                str = paste(str, ',', sep="")
            str = paste(str, deparse(n), ":", toJSON(x[[n]]), sep="")
        }
        str = paste( str, "}", sep="" )
        return( str )
    }

    #treat lists without names as JSON array
    if( length(x) != 1 || is.list(x) ) {
        if( !is.null(names(x)) )
            return( toJSON(as.list(x)) ) #vector with names - treat as JSON list
        str = "["
        first_elem = TRUE
        for( val in x ) {
            if( first_elem )
                first_elem = FALSE
            else
                str = paste(str, ',', sep="")
            str = paste(str, toJSON(val), sep="")
        }
        str = paste( str, "]", sep="" )
        return( str )
    }

    if( is.nan(x) )
        return( "\"NaN\"" )

    if( is.na(x) )
        return( "\"NA\"" )

    if( is.infinite(x) )
        return( ifelse( x == Inf, "\"Inf\"", "\"-Inf\"" ) )

    if( is.logical(x) )
        return( ifelse(x, "true", "false") )

    if( is.character(x) )
        return( gsub("\\/", "\\\\/", deparse(x)) )

    if( is.numeric(x) )
        return( as.character(x) )

    stop( "shouldnt make it here - unhandled type not caught" )
}

#create an object, which can be used to parse JSON data spanning multiple buffers
#it will be able to pull out multiple objects.. e.g: "[5][2,1]" is two different JSON objects - it can be called twice to get both items
newJSONParser <- function( method = "R" )
{
    if( method == "R" ) {
        buffer <- c()
        return( list(
            "addData" = function( buf ) {
                chars = strsplit(buf, "")[[1]]
                for( ch in chars )
                    buffer[ length(buffer) + 1 ]  <<- ch
            },
            "getObject" = function()
            {
                tmp <- .parseValue( buffer, 1)
                if( is.null( tmp$incomplete ) == FALSE )
                    return( NULL )

                if( tmp$size > length(buffer) )
                    buffer <<- c()
                else
                    buffer <<- buffer[ tmp$size : length(buffer) ]

                return( tmp$val )
            }
        ) )
    } else if( method == "C" ) {
        buffer <- ""
        return( list(
            "addData" = function( buf ) {
                buffer <<- paste( buffer, buf, sep="" )
            },
            "getObject" = function()
            {
                tmp <- .Call("fromJSON", buffer, PACKAGE="rjson")
                if( any( class( tmp[[ 1 ]] ) == "incomplete" ) )
                    return( NULL )

                size <- tmp[[ 2 ]] + 1

                buffer <<- substring( buffer, size, nchar( buffer ) )
                return( tmp[[ 1 ]] )
            }
        ) )
    }
    stop("bad method - only R or C" )
}


fromJSON <- function( json_str, file, method = "C" )
{
    if( missing( json_str ) ) {
        if( missing( file ) )
            stop( "either json_str or file must be supplied to fromJSON")
        json_str <- paste(readLines( file ),collapse="")
    } else {
        if( missing( file ) == FALSE ) {
            stop( "only one of json_str or file must be supplied to fromJSON")
        }
    }

    if( method == "R" )
        return( .fromJSON_R( json_str ) )
    if( method != "C" )
        stop( "only R or C method allowed" )

    x <- .Call("fromJSON", json_str, PACKAGE="rjson")[[ 1 ]]
    if( any( class(x) == "try-error" ) )
        stop( x )
    return( x )
}

.fromJSON_R <- function( json_str )
{
    if( !is.character(json_str) )
        stop( "JSON objects must be a character string" )
    chars = strsplit(json_str, "")[[1]]
    tmp <- .parseValue( chars, 1)
    if( is.null( tmp$incomplete ) )
        return( tmp$val )
    else
        return( NULL )
}

.parseValue <- function( chars, i )
{
    if( i > length( chars ) )
        return( list( "incomplete" = TRUE ) )

    #ignore whitespace
    while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
        i = i + 1
        if( i > length( chars ) )
            return( list( "incomplete" = TRUE ) )
    }

    ch = chars[i]
    if( ch == "{" ) {
        return( .parseObj( chars, i ) )
    }
    if( ch == "[" ) {
        return( .parseArray( chars, i ) )
    }
    if( ch == "\"" ) {
        return( .parseString( chars, i ) )
    }
    if( any(grep("[0-9\\-]", ch)) ) {
        return( .parseNumber( chars, i ) )
    }
    if( ch == "t" ) {
        return( .parseTrue( chars, i ) )
    }
    if( ch == "f" ) {
        return( .parseFalse( chars, i ) )
    }
    if( ch == "n" ) {
        return( .parseNull( chars, i ) )
    }
    #stop("shouldnt reach end of parseValue")

    err <- paste( "unexpected data:", paste( chars[ i:length(chars)], collapse = "" ) )
    stop( err )
}

.parseObj <- function( chars, i )
{
    obj <- list()
    if( chars[i] != "{" ) stop("error - no openning tag")
    i = i + 1
    if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )

    first_pass <- TRUE
    while( TRUE ) {

        #ignore whitespace
        while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
            i = i + 1
            if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
        }


        #look out for empty lists
        if( chars[i] == "}" && first_pass == TRUE ) {
            i = i + 1
            break
        }
        first_pass <- FALSE

        #get key
        str = .parseString( chars, i )
        if( is.null( str$incomplete ) == FALSE ) return( str )
        key = str$val
        i = str$size
        if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )

        #ignore whitespace
        while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
            i = i + 1
            if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
        }


        #verify seperater
        if( chars[i] != ":" ) stop("error - no seperator")
        i = i + 1
        if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )


        #ignore whitespace
        while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
            i = i + 1
            if( i > length( chars ) )
                return( list( "incomplete" = TRUE ) )
        }


        #get value
        val = .parseValue( chars, i )
        if( is.null( val$incomplete ) == FALSE ) return( val )
        obj[key] <- list(val$val)
        i = val$size

        if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )

        #ignore whitespace
        while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
            i = i + 1
            if( i > length( chars ) )
                return( list( "incomplete" = TRUE ) )
        }

        if( chars[i] == "}" ) {
            i = i + 1
            break
        }
        if( chars[i] != "," ) stop("error - no closing tag")
        i = i + 1
        if( i > length( chars ) )
            return( list( "incomplete" = TRUE ) )
    }
    return( list(val=obj, size=i) )
}

.parseArray <- function( chars, i )
{
    useVect <- TRUE
    arr <- list()
    if( chars[i] != "[" ) stop("error - no openning tag")

    i = i + 1
    if( i > length( chars ) )
        return( list( "incomplete" = TRUE ) )

    while( TRUE ) {

        #ignore whitespace
        while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
            i = i + 1
            if( i > length( chars ) )
                return( list( "incomplete" = TRUE ) )
        }

        #look out for empty arrays
        if( chars[i] == "]" ) {
            i = i + 1
            useVect <- FALSE #force an empty list instead of NULL (i.e. value = vector("list",0))
            break
        }

        #get value
        val = .parseValue( chars, i )
        if( is.null( val$incomplete ) == FALSE ) return( val )
        arr[length(arr)+1] <- list(val$val)
        if( is.list(val$val) || length(val$val) > 1 || is.null(val$val) )
            useVect <- FALSE

        i = val$size
        if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )

        #ignore whitespace
        while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" ) {
            i = i + 1
            if( i > length( chars ) )
                return( list( "incomplete" = TRUE ) )
        }

        if( chars[i] == "]" ) {
            i = i + 1
            break
        }
        if( chars[i] != "," ) stop("error - no closing tag")
        i = i + 1
        if( i > length( chars ) )
            return( list( "incomplete" = TRUE ) )
    }
    if( useVect )
        arr <- unlist(arr)
    return( list(val=arr, size=i) )
}

.parseString <- function( chars, i )
{
    str_start = i
    if( chars[i] != "\"") stop("error")
    i = i + 1
    if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )

    while( TRUE ) {
        while( chars[i] != "\\" && chars[i] != "\"" ) {
            i = i + 1
            if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
        }
        if( chars[i] == "\\" ) {
            i = i + 2 #skip the next char
            if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
        }
        else
            break
    }
    str_end = i
    i = i + 1
    return(list(
        val=eval(parse( text=paste(chars[str_start:str_end], collapse="") )),
        size=i ))
}

.parseNumber <- function( chars, i )
{
    str_start = i

    if( chars[i] == "-" )
        i = i + 1

    if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )

    if( chars[i] == "0" ) {
        i = i + 1
        if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
        if( any(grep("[1-9]", chars[i])) ) stop("JSON specs don't allow a number like \"012\"")
    } else if( any(grep("[1-9]", chars[i])) ) {
        i = i + 1
        if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
        while( any(grep("[0-9]", chars[i])) ) {
            i = i + 1
            if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
        }
    } else {
        stop( "doesn't look like a valid JSON number" )
    }

    if( chars[i] == "." ) {
        i = i + 1
        if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
        while( any(grep("[0-9]", chars[i])) ) {
            i = i + 1
            if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
        }
    }

    if( chars[i] == "e" || chars[i] == "E" ) {
        i = i + 1
        if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
        if( chars[i] == "-" || chars[i] == "+" )
            i = i + 1
        if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
        while( any(grep("[0-9]", chars[i])) ) {
            i = i + 1
            if( i > length( chars ) ) return( list( "incomplete" = TRUE ) )
        }
    }
    str_end = i-1

    return(list(
        val=eval(parse( text=paste(chars[str_start:str_end], collapse="") )),
        size=i ))
}

.parseTrue <- function( chars, i )
{
    if( paste(chars[i:(i+3)], collapse="") == "true" )
        return( list(val=TRUE,size=i+4) )
    stop("error parsing true value (maybe the word starts with t but isnt true)")
}

.parseFalse <- function( chars, i )
{
    if( paste(chars[i:(i+4)], collapse="") == "false" )
        return( list(val=FALSE,size=i+5) )
    stop("error parsing false value (maybe the word starts with f but isnt false)")
}

.parseNull <- function( chars, i )
{
    if( paste(chars[i:(i+3)], collapse="") == "null" )
        return( list(val=NULL,size=i+4) )
    stop("error parsing null value (maybe the word starts with n but isnt null)")
}

  regex_break <- function(whole_regex, regexes, source) {
    whole_matches <- gregexpr(paste("(?sm)", whole_regex, sep=""), source, perl=TRUE)[[1]]
    whole_matches <- mapply(
        function(start, end) substr(source, start, end),
        whole_matches,
        whole_matches + attr(whole_matches, "match.length") - 1
    )
    m <- length(whole_matches)
    n <- length(regexes)
    matches <- matrix("", nrow=m, ncol=n)
    for (i in 1:m) {
        whole <- whole_matches[i]
        for (j in 1:length(regexes)) {
            rest_regex <- paste(regexes[-(1 : j)], collapse="")
            part_regex <- paste("(?sm)\\A", regexes[j], "(?=", rest_regex, "\\Z)", sep="")
            match <- regexpr(part_regex, whole, perl=TRUE)
            end <- attr(match, "match.length")
            matches[i, j] <- substr(whole, 1, end)
            whole <- substr(whole, end + 1, nchar(whole))
        }
    }
    matches
}

strip <- function(str) gsub("^\\s+|\\s+$", "", str)
rstrip <- function(str) gsub("\\s+$", "", str)

super_strip <- function(str) {
    str <- gsub("(^|\n)# ?", "\n", str)
    str <- gsub("\\A\\s+|\\s+\\Z", "", str, perl=TRUE)
}

get_current_filename <- function () {
  if (length(showConnections()) > 1) {
    return(showConnections()[1, "description"])
  } else {
    return(Find(Negate(is.null), Map(function(f) { f$ofile }, sys.frames()), right=TRUE))
  }
}

postJSON <-function(host, path, port=80, json) {
  con <- socketConnection(host=host, port=port, server=FALSE, blocking=TRUE)
  post <- paste(
    "POST ", path, " HTTP/1.1\r\n",
    "Host: ", host, "\r\n",
    "Connection: close\r\n",
    "Content-Type: application/json; charset=utf-8\r\n",
    "Content-Length: ", nchar(json, type="bytes"), "\r\n\r\n",
    json,
    sep = ""
  )
  writeLines(post, con=con, sep="", useBytes=TRUE)
  response <- paste(readLines(con, warn=FALSE), collapse="\r\n")
  close.connection(con)
  Encoding(response) <- "UTF-8"

  header <- sub("\r\n\r\n.*?$", "", response)
  if(grepl("Transfer-Encoding: chunked", header)) {
    chunked <- sub("^.*?\r\n\r\n", "", response)
    contents <- ""
    repeat {
      match <- regex_break(".*", c("[a-f0-9]+", "\\r\\n", ".*"), chunked)
      len <- strtoi(match[1, 1], 16)
      rest <- match[1, 3]
      if(len == 0 || ncol(match) == 0)
        break
      contents <- paste(contents, substr(rest, 1, len), sep = "")
      chunked <- substr(rest, len + 2, nchar(rest))
    }
  } else {
    contents <- sub("^.*?\r\n\r\n", "", response)
  }
  return(contents)
}

pretty.print <- function(x) {
  output <- capture.output(print(x))
  if(length(output) == 0) {
    return("NULL")
  } else if(length(output) == 1) {
    return(output)
  } else {
    return(paste("    ", c("", output, ""), collapse = "\n"))
  }
}


  check <- list()

check$initialize <- function(parts) {
  init.part <- function(part) {
    part$errors <- list()
    part$challenge <- list()
    return(part)
  }
  check$parts <<- lapply(parts, init.part)
  check$current <<- NA
  check$part.counter <<- NA
}

check$part <- function() {
  if(is.na(check$part.counter)) {
    check$part.counter <<- 1
  } else {
    check$part.counter <<- check$part.counter + 1
  }
  return(strip(check$parts[[check$part.counter]]$solution) != "")
}

check$error <- function(msg, ...) {
  check$parts[[check$part.counter]]$errors <<-
    c(check$parts[[check$part.counter]]$errors, sprintf(msg, ...))
}

check$challenge <- function(x, k = NA) {
  pair <- c(toString(k), toString(check$canonize(x)))
  check$parts[[check$part.counter]]$challenge<<-
    c(check$parts[[check$part.counter]]$challenge, list(pair))
}

check$run <- function(example, state) {
  # yet to be implemented
}

check$canonize <- function(x, digits = 6) {
  if(typeof(x) == "double" || typeof(x) == "complex") {
    return(round(x, digits))
  } else if(typeof(x) == "complex") {
    return(round(x, digits))
  } else if(typeof(x) == "list") {
    return(lapply(x, function(y) check$canonize(y, digits)))
  } else {
    return(x)
  }
}

check$equal <- function(example, value = NA, exception = NA,
                        clean = function(x) x,
                        precision = 1.0e-6, strict.float = FALSE, check.attributes = FALSE) {
  difference <- function(x, y) {
    if(identical(x, y)) return(NA)
    else if(isTRUE(all.equal(x, y, check.attributes = check.attributes))) return(NA)
    else if(typeof(x) != typeof(y) && (strict.float || !(mode(x) != mode(y))))
      return("različna tipa")
    else if(length(x) != length(y))
      return("različno število komponent")
    else if(mode(x) == 'numeric' && mode(y) == 'numeric') {
      if(any(abs(x - y) > precision))
        return("numerična napaka")
      else
        return(NA)
    }
    else return("različni vrednosti")
  }
  example <- substitute(example)

  if(!is.na(exception)) {
    tryCatch({
      eval(example)
      check$error("Izraz %s vrne vrednost namesto da bi sprožil izjemo '%s'.",
                  deparse(example), exception)
    }, error = function(e) {
      if(e$message != exception)
        check$error("Izraz %s sproži izjemo '%s' namesto '%s'.",
                    deparse(example), e$message, exception)
    })
  } else {
    returned <- eval(example)
    reason <- difference(clean(returned), clean(value))
    if(!is.na(reason)) {
      check$error("Izraz %s vrne %s namesto %s (%s)",
                  deparse(example), pretty.print(returned), pretty.print(value), reason)
    }
  }
}

check$random <- function(example, period = 10, sample = 100, uniqueness = 0.9) {
  example <- substitute(example)
  results <- replicate(sample, toString(check$canonize(replicate(period, eval(example)))))
  if (length(unique(results)) < uniqueness * sample) {
    check$error("Izraz %s ne vrača naključnih rezultatov.", deparse(example))
  }
}

check$probability <- function(example, interval, sample = 100) {
  example <- substitute(example)
  results <- replicate(sample, isTRUE(eval(example)))
  prob <- sum(results) / sample
  if (!(interval[1] < prob && prob <= interval[2])) {
    check$error("Izraz %s velja z verjetnostjo %.2f, ki je izven pričakovanega intervala [%.2f, %.2f].", deparse(example), prob, interval[1], interval[2])
  }
}

check$expected <- function(example, interval, sample = 100) {
  example <- substitute(example)
  results <- replicate(sample, eval(example))
  prob <- sum(results) / sample
  if (!(interval[1] < prob && prob <= interval[2])) {
    check$error("Povprečna vrednost izraza %s je %.2f, kar je izven pričakovanega intervala [%.2f, %.2f].", deparse(example), prob, interval[1], interval[2])
  }
}

check$summarize <- function() {
  for(i in 1:length(check$parts)) {
    if(strip(check$parts[[i]]$solution) == "") {
      cat("Podnaloga", i, "je brez rešitve.\n")
    } else if (length(check$parts[[i]]$errors) > 0) {
      cat("Podnaloga", i, "ni prestala vseh testov:\n")
      cat(paste("- ", check$parts[[i]]$errors, "\n", sep = ""), sep = "")
    } else if ("rejection" %in% names(check$parts[[i]])) {
      cat("Podnaloga ", i, " je zavrnjena. (", check$parts[[i]]$rejection, ")\n", sep = "")
    } else {
      cat("Podnaloga", i, "je pravilno rešena.\n")
    }
  }
}


  .source <- paste(readLines(.filename), collapse="\n")

  matches <- regex_break(paste(
      '#+@(\\d+)#\n', # beginning of header
      '.*?',          # description
      '#+\\1@#\n',    # end of header
      '.*?',          # solution
      '(?=#+@)',      # beginning of next part
      sep=""
  ),  c(
      '#+@',          # beginning of header
      '(\\d+)',       # beginning of header (?P<part>)
      '#\n',          # beginning of header
      '.*?',          # description
      '#+(\\d+)@#\n', # end of header
      '.*?'           # solution
  ), .source)

  check$initialize(
    apply(matches, 1, function(match) list(
        part = as.numeric(match[2]),
        solution = match[6]
      )
    )
  )
  check$parts[[length(check$parts)]]$solution = rstrip(check$parts[[length(check$parts)]]$solution)

  problem_match <- regex_break(paste(
    '#+@@#\n', # beginning of header
    '.*?',     # description
    '#+@@#\n', # end of header
    '.*?',     # preamble
    '(?=#+@)', # beginning of first part
    sep = ""
  ), c(
    '#+@@#\n', # beginning of header
    '.*?',     # description
    '#+@@#\n', # end of header
    '.*?'      # preamble
    ), .source)

  if(length(problem_match) == 0)
    stop("NAPAKA: datoteka ni pravilno oblikovana")

  .preamble <- problem_match[1, 4]

  
  if (check$part()) {
    tryCatch({
      if(!file.exists("gdp.csv")) {
        cat("Prenašam datoteko \"gdp.csv\"... ")
        download.file("http://pastebin.com/raw.php?i=L1ftrjiX", "gdp.csv", quiet=TRUE)
        cat("Datoteka je prenešena.\n")
      }
      if(!file.exists("population.csv")) {
        cat("Prenašam datoteko \"population.csv\"... ")
        download.file("http://pastebin.com/raw.php?i=2Jj2Q9zP", "population.csv", quiet=TRUE)
        cat("Datoteka je prenešena.\n")
      }
      gdp <- uvozi("gdp.csv")
      check$equal(nrow(gdp), 259)
      check$equal(ncol(gdp), 211)
      population <- uvozi("population.csv")
      check$equal(nrow(population), 259)
      check$equal(ncol(population), 232)
      if(is.null(rownames(gdp))) {
        check$error("Uvažanje pobriše imena vrstic.")
      } else if(!("Slovenia" %in% rownames(gdp))) {
        check$error("Uvažanje pokvari imena vrstic.")
      } else if(is.null(colnames(gdp))) {
        check$error("Uvažanje pobriše imena stolpcev.")
      } else if(!("X1982" %in% colnames(gdp))) {
        check$error("Uvažanje pokvari imena stolpcev.")
      } else {
        check$equal(gdp["Slovenia", "X2010"], 25040.68, precision=2)
        check$equal(gdp["United Kingdom", "X1950"], 9766.504, precision=3)
        check$equal(gdp["Ethiopia", "X1970"], 539.8842, precision=4)
        check$equal(gdp["Cote d'Ivoire", "X2000"], 1746.313, precision=3)
        check$equal(gdp["Yemen, Rep.", "X1990"], 1923.3, precision=1)
        check$equal(population["Slovenia", "X2010"], 2029680, precision=2)
        check$equal(population["United Kingdom", "X1950"], 50616012, precision=3)
        check$equal(population["Ethiopia", "X1970"], 28959382, precision=4)
        check$equal(population["Cote d'Ivoire", "X2000"], 16581653, precision=3)
        check$equal(population["Yemen, Rep.", "X1990"], 11948209, precision=1)
      }
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
  }
  
  if (check$part()) {
    tryCatch({
      x <- data.frame(
        X1900 = c(10, 200, 3000, NA),
        X1950 = c(20, NA, NA, NA),
        X1975 = c(40, NA, 4000, NA),
        X2000 = c(65, 500, NA, NA),
        row.names = c("Elbonia", "Eurasia", "Eastasia", "Amnesia")
      )
      y <- data.frame(
        X1900 = c(10, 200, 3000, NA),
        X1950 = c(20, 350, 3666.66666666667, NA),
        X1975 = c(40, 425, 4000, NA),
        X2000 = c(65, 500, NA, NA),
        row.names = c("Elbonia", "Eurasia", "Eastasia", "Amnesia")
      )
      z <- dopolni(x)
      if(!isTRUE(all.equal(as.matrix(y), as.matrix(z)))) {
        check$error("Razpredelnica %s se ne dopolni do %s temveč do %s", pretty.print(x), pretty.print(y), pretty.print(z))
      }
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
  }
  
  if (check$part()) {
    tryCatch({
      
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
  }
  
  if (check$part()) {
    tryCatch({
      
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
  }
  

  
  cat('Shranjujem rešitve na strežnik... ')
  post <- list(
    data = '{"timestamp": "2012-01-17 20:55:41.740258", "problem": 92, "user": 56}',
    signature = 'a743a6edba3317a0ac402ae9a358ba7f',
    preamble = .preamble,
    attempts = check$parts,
    source = "" # sending source somehow causes problems on the server side.
  )
  tryCatch({
    r <- postJSON(host='tomo.fmf.uni-lj.si', path='/problem/upload/student/', port=80, json=enc2utf8(toJSON(post)))
    response <- fromJSON(r, method = "R")
    cat('Rešitve so shranjene.\n')
    for(rejected in response$rejected)
      check$parts[[as.integer(rejected[[1]])]]$rejection <- rejected[[2]]
    check$summarize()
    if("update" %in% names(response)) {
      cat("Posodabljam datoteko... ")
      index <- 1
      while(file.exists(paste(.filename, ".", index, sep = "")))
        index <- index + 1
      backup.filename = paste(.filename, ".", index, sep = "")
      file.copy(.filename, backup.filename)
      r <- readLines(response$update, encoding="UTF-8", warn=FALSE)
      f <- file(.filename, encoding="UTF-8")
      writeLines(r, f)
      close.connection(f)
      cat("Stara datoteka je preimenovana v ", basename(backup.filename), ".\n", sep = "")
      cat("Če se datoteka v urejevalniku ni osvežila, jo shranite ter ponovno zaženite.\n")
    }
  },
  error = function(r) {
    cat('Pri shranjevanju je prišlo do napake.\n')
    check$summarize()
    cat('Pri shranjevanju je prišlo do napake. Poskusite znova.\n')
  })
  
}

.check()
