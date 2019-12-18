# SCI01 Stanford - Fall 2016

library(RSelenium)

# run the selenium server and open firefox browser
RSelenium::startServer()
remDr <- remoteDriver$new()
require(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost" ,
                       port = 4444,
                       browserName = "safari"
)
remDr$open()


# Navigation
remDr$navigate("http://www.google.com")
remDr$getCurrentUrl()

# Accessing elements in the DOM (Document Object Model)

webElem <- remDr$findElement(using = 'id', "lst-ib")
webElem$sendKeysToElement(list("how old is Obama?"))
webElem <- remDr$findElement(using = 'name', "btnG")
webElem$clickElement()


