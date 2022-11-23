Win2OpenBUGS <- function(WinBugsScriptFile,OpenBugsScriptFile)
{
###  Translate a WinBUGS script file to an OpenBUGS script file.
###
###  The first argument is a character string giving the name of a text file
###  containing a WinBUGS script, e.g., "c:/temp/wbscript.txt".  The second
###  argument is a character string giving the name of a text file to which
###  the corresponding OpenBUGS script will be written, e.g.,
###  "c:/temp/obscript.txt".
###
###  Chris Jackson <chris.jackson@mrc-bsu.cam.ac.uk> April 2010

    cmds <-
        rbind(
              c("compile","modelCompile"),
              c("gen.inits","modelGenInits"),
              c("data","modelData"),
              c("check","modelCheck"),
              c("display","modelDisplay"),
              c("quit","modelQuit"),
              c("save","modelSaveLog"),
              c("set.rank","ranksSet"),
              c("stats.rank","ranksStats"),
              c("hist.rank","ranksHistogram"),
              c("clear.rank","ranksClear"),
              c("set","samplesSet"),
              c("thin.samples","samplesThin"),
              c("density","samplesDensity"),
              c("autoC","samplesAutoC"),
              c("history","samplesHistory"),
              c("stats","samplesStats"),
              c("trace","samplesTrace"),
              c("coda","samplesCoda"),
              c("quantiles","samplesQuantiles"),
              c("gr","samplesBgr"),
              c("beg","samplesBeg"),
              c("end","samplesEnd"),
              c("first","samplesFirstChain"),
              c("last","samplesLastChain"),
              c("set.summary","summarySet"),
              c("stats.summary","summaryStats"),
              c("mean.summary","summaryMean"),
              c("clear.summary","summaryClear"),
              c("dic.set","dicSet"),
              c("dic.stats","dicStats")
              )

    ## Most commands are simple drop-in replacements of their WinBUGS equivalents.
    ## Preserve any space between command and open bracket.
    ## Commands may be commented out or have whitespace before them
    OpenBugsScript <- readLines(WinBugsScriptFile)
    for (i in 1:nrow(cmds)) {
        win.cmd <- paste("^([[:space:]#]*)", cmds[i,1], "([[:space:]]*\\(.*\\).*)", sep="")
        open.cmd <- paste("\\1", cmds[i,2], "\\2", sep="")
        OpenBugsScript <- gsub(win.cmd, open.cmd, OpenBugsScript)
    }

    ## modelInits(file, chain) instead of inits(chain, file)
    ## preserve existing white-space formatting
    sqstring <- "\\'[^\\']*\\'"   # single-quoted file name. spaces are meaningful in filenames
    win.cmd <- paste("inits([[:space:]]*\\([[:space:]]*)([^[:space:]]*)([[:space:]]*,[[:space:]]*)(",sqstring,")([[:space:]]*\\))",sep="")
    open.cmd <- "modelInits\\1\\4\\3\\2\\5"
    OpenBugsScript <- gsub(win.cmd, open.cmd, OpenBugsScript)
    dqstring <- '\"[^\"]*\"'   # double-quoted file name.
    win.cmd <- paste("inits([[:space:]]*\\([[:space:]]*)([^[:space:]]*)([[:space:]]*,[[:space:]]*)(",dqstring,")([[:space:]]*\\))",sep="")
    OpenBugsScript <- gsub(win.cmd, open.cmd, OpenBugsScript)

    ## modelUpdate(1000, 2, 10, 'T')  instead of
    ## update(1000); thin.updater(2); refresh(10); over.relax('T')
    ## set thin to 1 if no thin.updater command or it's commented out
    ## set refresh to 100 if no refresh command or it's commented out
    thin.ind <- grep("^[^#]*thin\\.updater[[:space:]]*\\(.+\\).*", OpenBugsScript)
    refresh.ind <- grep("^[^#]*refresh[[:space:]]*\\(.+\\).*", OpenBugsScript)
    overrelax.ind <- grep("^[^#]*over\\.relax[[:space:]]*\\(.+\\).*", OpenBugsScript)
    thin <-
        if (length(thin.ind) == 0) 1
        else gsub("^[^#]*thin\\.updater[[:space:]]*\\((.+)\\).*", "\\1", OpenBugsScript[thin.ind][1])
    refresh <-
        if (length(refresh.ind) == 0) 100
        else gsub("^[^#]*refresh[[:space:]]*\\((.+)\\).*", "\\1", OpenBugsScript[refresh.ind][1])
    overrelax <-
        if (length(overrelax.ind) == 0) "\'F\'"
        else gsub("^[^#]*over\\.relax[[:space:]]*\\((.+)\\).*", "\\1", OpenBugsScript[overrelax.ind][1])
    if (any(length(thin.ind),length(refresh.ind),length(overrelax.ind)))
        OpenBugsScript <- OpenBugsScript[-c(thin.ind,refresh.ind,overrelax.ind)]
    win.cmd <- "update([[:space:]]*\\(.*)\\)"
    open.cmd <-
        if (length(overrelax.ind) > 0)
            paste("modelUpdate\\1,",thin,",",refresh,",",overrelax,")",sep="")
        else if (length(refresh.ind) > 0)
            paste("modelUpdate\\1,",thin,",",refresh,")",sep="")
        else if (length(thin.ind) > 0)
            paste("modelUpdate\\1,",thin,")",sep="")
        else "modelUpdate\\1)"
    OpenBugsScript <- gsub(win.cmd, open.cmd, OpenBugsScript)

    ## double-quote any stars supplied as node wildcards
    ## only quote stars which are not already single or double quoted
    ## preserve existing white-space formatting
    OpenBugsScript <- gsub("([^\"\'[:space:]][[:space:]]*)\\*([[:space:]]*[^\"\'[:space:]])", "\\1\"\\*\"\\2", OpenBugsScript)

    writeLines(OpenBugsScript,OpenBugsScriptFile)
    OpenBugsScript
}
