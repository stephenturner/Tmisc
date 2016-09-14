#'Play a short sound
#'
#'\code{beep} plays a short sound which is useful if you want to get notified, 
#'for example, when a script has finished. Taken from the \code{beepr} package.
#'
#'If \code{beep} is not able to play the sound a warning is issued rather than 
#'an error. This is in order to not risk aborting or stopping the process that
#'you wanted to get notified about.
#'
#'@param expr An optional expression to be excecuted before the sound.
#'  
#'@return NULL
#'
#' @import audio
#'  
#' @examples
#' \dontrun{
#' # Play a beep sound
#' beep()
#'
#' 
#' # Update all packages and "ping" when it's ready
#' update.packages(ask=FALSE); beep()
#' }
#'@export
beep <- function(expr=NULL) {
    expr
    if(Sys.info()["sysname"] == "Linux") {
        warning("beep() doesn't work on Linux.")
    } else {
        sound_path <- system.file("sounds/beep.wav", package="Tmisc")
        # sound_path <- "inst/sounds/beep.wav"
        tryCatch(play(load.wave(sound_path)), 
                 error = function(ex) warning("beep() could not play the sound due to the following error:\n", ex))
    }
}