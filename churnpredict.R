churnpredict <- function(group, totalage, margin, oncontract, offcontractmonths)
{
    churn <- .03
    if(group==1)
    {
        churn <- churn * 1.5
    }else if (group==2)
    {
        churn <- churn * 1.25
    }else if (group == 3)
    {
        churn <- churn * 1
    }else if (group == 4)
    {
        churn <- churn * .75
    }else if (group == 5)
    {
        churn <- churn * .5
    }
    
    
    churn <- churn * max((144-totalage)/144,0)
    
    churn <- churn * (100+margin/10)/100
    
    if(oncontract)
    {
        churn <- churn/5
    }else
    {
        churn <- churn * max((120-offcontractmonths)/120,0)
    }
    max(churn,.005)
}