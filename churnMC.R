visualizechurn <- function()
{
    ptm <- proc.time() #start the timer
    
    library(ggplot2)
    library(png)
    library(scales)
    
    # Monte Carlo Simulation of remaining lifetime value of
    # the entire book of customer as represented by the file
    # currentcustomers.  100 simulations on 100 customers
    path="~/ChurnDemo/"
    filename="currentcustomers.csv"
    bookdf <- booksimulation(numexperiments=1000, seed=42, paste0(path,filename))
    konopng <- png::readPNG(paste0(path,"Kono-Logo-Black.320x121.png"))
    bookdf <- aggregate(x=list(totalmargin=bookdf$totalmargin),by=list(experiment=bookdf$experiment),FUN=sum)

    konopng <- png::readPNG(paste0(path,"Kono-Logo-Black.320x121.png"))

    h1 <- ggplot(data=bookdf, aes(bookdf$totalmargin/1000))
    h1 <- h1 + geom_histogram(col="purple", 
                       fill="gold", 
                       alpha = .2,
                       binwidth = 20)
    h1 <- h1 + labs(title="Portfolio Lifetime Value Histogram")
    h1 <- h1 + labs(x="Lifetime Value (in thousands of dollars)", y="Number of Scenarios in Monte Carlo Simulation")
    h1 <- h1 + scale_x_continuous(labels = scales::dollar, breaks = seq(0,max(bookdf$totalmargin),10))
    h1 <- h1 + theme(axis.text.x = element_text(angle = 90))
    h1 <- h1 + annotation_raster(konopng, ymin= 275, ymax = 350, xmin = 200, xmax = 250)
    h1 <- h1 + annotate("text", x=225, y=250, label = paste0("median = ",scales::dollar_format(largest_with_cents = 1)(median(bookdf$totalmargin))))
    h1 <- h1 + annotate("text", x=225, y=235, label = paste0("mean = ",scales::dollar_format(largest_with_cents = 1)(mean(bookdf$totalmargin))))
    h1 <- h1 + annotate("text", x=225, y=220, label = paste0("max = ",scales::dollar_format(largest_with_cents = 1)(max(bookdf$totalmargin))))
    h1 <- h1 + annotate("text", x=225, y=205, label = paste0("min = ",scales::dollar_format(largest_with_cents = 1)(min(bookdf$totalmargin))))
    h1 <- h1 + annotate("text", x=225, y=190, label = paste0("sd = ",scales::dollar_format(largest_with_cents = 1)(sd(bookdf$totalmargin))))
    ggsave(filename=paste0(path,"PortfolioLV.hist.png"), plot=h1, width=5, height=5, units = "in")

    #Monte Carlo Simulation of remaining lifetime value of 
    #a customer on month 20 of a 24 month contract with a 
    #$10 margin.  1000 simulations on one customer
    customerdf <- customersimulation(numexperiments = 1000,
                                     group = 1,
                                     totalage=20,
                                     margin=10,
                                     oncontract=TRUE,
                                     contractmonthsleft = 4,
                                     offcontractmonths = 0)
    h2 <- ggplot(data=customerdf[customerdf$totalmargin<750,], aes(customerdf$totalmargin[customerdf$totalmargin<750]))
    h2 <- h2 + geom_histogram(col="purple", 
                       fill="gold", 
                       alpha = .2,
                       binwidth=50)
    h2 <- h2 + labs(title="One Customer Lifetime Value Histogram")
    h2 <- h2 + labs(x="Lifetime Value (in dollars)", y="Number of Scenarios in Monte Carlo Simulation")
    h2 <- h2 + scale_x_continuous(labels = scales::dollar)
    h2 <- h2 + theme(axis.text.x = element_text(angle = 90))
    h2 <- h2 + annotation_raster(konopng, ymin = 110, ymax = 125, xmin = 500, xmax = 650)
    h2 <- h2 + annotate("text", x=575, y=100, label = paste0("median = ",scales::dollar_format(largest_with_cents = 1)(median(customerdf$totalmargin))))
    h2 <- h2 + annotate("text", x=575, y=92, label = paste0("mean = ",scales::dollar_format(largest_with_cents = 1)(mean(customerdf$totalmargin))))
    h2 <- h2 + annotate("text", x=575, y=84, label = paste0("max = ",scales::dollar_format(largest_with_cents = 1)(max(customerdf$totalmargin))))
    h2 <- h2 + annotate("text", x=575, y=76, label = paste0("min = ",scales::dollar_format(largest_with_cents = 1)(min(customerdf$totalmargin))))
    h2 <- h2 + annotate("text", x=575, y=68, label = paste0("sd = ",scales::dollar_format(largest_with_cents = 1)(sd(customerdf$totalmargin))))
    ggsave(filename=paste0(path,"CustomerLV.hist.png"), plot=h2, width=5, height=5, units = "in")
    
    print("Execution Time:")
    print(proc.time()-ptm)
}

booksimulation <- function(numexperiments, seed=NA, fileandpath="~/ChurnDemo/currentcustomers.csv")
{
    bookdf <- read.csv(fileandpath, stringsAsFactors = FALSE)
    dfout <- data.frame(customerid=numeric(), experiment=integer(), totalmonths=numeric())
    for (i in 1:nrow(bookdf))
    {
        customersimdf <- customersimulation(numexperiments=numexperiments,
                                            group=bookdf$group[i],
                                            totalage=bookdf$totalage[i],
                                            margin=bookdf$margin[i],
                                            oncontract=bookdf$oncontract[i],
                                            contractmonthsleft=bookdf$contractmonthsleft[i],
                                            offcontractmonths=bookdf$offcontractmonths[i])
        customersimdf$customerid <- bookdf$customerid[i]
        dfout <- rbind(dfout, customersimdf)
    }
    dfout
}

customersimulation <- function(numexperiments, seed=NA, group, totalage, margin, oncontract, contractmonthsleft, offcontractmonths)
{
    if (!is.na(seed))
    {
        set.seed(seed)
    }

    dfout <- data.frame(experiment=integer(), totalmonths=numeric())
    for (i in 1:numexperiments)
    {
        customerdf <- customerexperiment(group, totalage, margin, oncontract, contractmonthsleft, offcontractmonths)
        dfout <- rbind(dfout,data.frame(experiment=i, totalmonths=nrow(customerdf), totalmargin=sum(customerdf$margin)))
    }
    dfout
}

customerexperiment <- function(group, totalage, margin, oncontract, contractmonthsleft, offcontractmonths)
{
    source("~/ChurnDemo/churnpredict.R")
    
    churnpercent <- churnpredict(group, totalage, margin, oncontract, offcontractmonths)
    churned <- FALSE
    i <- 1
    dfout  <- data.frame(month=integer(),margin=numeric(),churnpercent=numeric())
    while (!churned)
    {
        dfout <- rbind(dfout,data.frame(month=i,margin=margin,churnpercent))
        i <- i + 1
        totalage <- totalage + 1
        if(contractmonthsleft>0)
        {
            contractmonthsleft <- contractmonthsleft - 1
        }else
        {
            oncontract <- FALSE
            offcontractmonths <- offcontractmonths + 1
        }
        churnpercent <- churnpredict(group, totalage, margin, oncontract, offcontractmonths)
        churned <- willchurn(churnpercent)
    }
    dfout
}

willchurn <- function(churnpercent)
{
    runif(1) <= churnpercent
}

