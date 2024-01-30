make_dir <- function(path){
  if( !dir.exists(path) ){
    dir.create(path)
  }
}
