DS.parametersFromCharacteristics <- function(b,discrimination,o = DS.options())
{
  if ( o$model == 1 ) {
    return( b )
  } else {
    s = 4 * discrimination * (1 - b) * b;
    return( c(b,s));
  }
}