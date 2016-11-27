visualization <- function()
{
    library(ggplot2)
    
    # Monte Carlo Simulation of remaining lifetime value of
    # the entire book of customer as represented by the file
    # currentcustomers.  100 simulations on 100 customers
    path="~/ChurnDemo/"
    filename="currentcustomers.csv"
    bookdf <- booksimulation(numexperiments=100, seed=42, paste0(path,filename))
    margindistribution <- aggregate(x=list(totalmargin=bookdf$totalmargin),by=list(experiment=bookdf$experiment),FUN=sum)
    ggplot(data=margindistribution, aes(margindistribution$totalmargin/1000)) + 
        geom_histogram(col="red", 
                       fill="green", 
                       alpha = .2,
                       bins=10) + 
        labs(title="Portfolio Lifetime Value Histogram") +
        labs(x="Lifetime Value (in thousands of dollars)", y="Number of Scenarios") +
        scale_x_continuous(labels = scales::dollar)
    ggsave(paste0(path,"PortfolioLV.hist.png"))
    margindistribution
    
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
    ggplot(data=customerdf, aes(customerdf$totalmargin)) + 
        geom_histogram(col="red", 
                       fill="green", 
                       alpha = .2,
                       binwidth=25) +
        xlim(c(0,500)) + 
        labs(title="One Customer Lifetime Value Histogram") +
        labs(x="Lifetime Value (in dollars)", y="Number of Scenarios")
    ggsave(paste0(path,"CustomerLV.hist.png"))
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

