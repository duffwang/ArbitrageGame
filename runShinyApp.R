message('Cutler Group LP\n\nAuthor: Jeff Wang\n\n')
message('Checking local library path:\n', paste('... ', .libPaths(), sep='', collapse='\n'))

library(shiny)

wd = getwd()

launch.browser = function(appUrl) {
  shell(sprintf('"%s" --app=%s', 
                file.path(wd,
                          'GoogleChromePortable/App/Chrome-bin/chrome.exe'), 
                appUrl))
}

shiny::runApp('./shiny/', launch.browser=launch.browser)