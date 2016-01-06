library(ggKaplanMeyer)
library(survival)
library(gtable)
library(grid)
library(ggplot2)
library(shinyjs)
library(colorspace)
ui <- shinyUI(fluidPage(

    titlePanel("Kaplan-Meier with ggplot2"),
    tabsetPanel(
        tabPanel("Plot",
            fluidRow(
                column(4,
                       wellPanel(
                        fileInput('data',label="Choose data on csv format (refer to as data)"),
                        textInput('survObject',label="Survobject (surv_object)"),
                        textInput('survFit',label="Survfit"),
                        #radioButtons("strata", label = "Strata",
                        #                   choices=list("Sex"="SEX","Current Smoker"="CURRENT_SMOKER","Ever a smoker"="EVER_SMOKER",Statins="STATINS","Overall"=1)),
        
        
                        checkboxInput("startPoint", label="Time starts at first data point", value=FALSE),
                        checkboxInput("timeInYears", label="Time in years", value=FALSE),
                        checkboxInput("n.risk", label="Add number at risk table", value=FALSE),
                        uiOutput('colors'),
                        checkboxInput("Advanced","Advanced settings",value=FALSE),
                        conditionalPanel(condition="input.Advanced==true",
                        h4("Plot properties"),
                        radioButtons("axisTicks", label = "Nr. of axis ticks",
                                     choices=list("1x"="1x","2x"="2x","4x"="4x")),
                        checkboxInput("background",label="Background"),
                        checkboxInput("confinterval", label="Include 95% confidence interval", value=TRUE),
                       textInput('title',label="Title"),
                       textInput('xlab',label="x-label"),
                       textInput('ylab',label="y-label"),
                       textInput('legendText',label="Names of strata"),
                       textInput('pvalue',label="p-value"),
                        radioButtons("legend", label = "Legend position",
                                     choices=list("None"="none","Bottom left"="bottom-left","Top left"="top-left","Top right"="top-right","Bottom right"="bottom-right"))
                        ),
        
                       
                        actionButton("go",label="Submit"),
                       downloadButton('download',label="Download plot as pdf")
                       )
                ),
                column(8,
                    textOutput("message"),
                    plotOutput("kmplot",click = "plot_click")         
                    #textOutput("debug")
                    
                )
            )
        ),
        tabPanel("Data",
            fluidRow(
                column(8,
                       tableOutput("dataHead")
                )
                
            )
                 
        )
    )
))
server <- function(input,output){
colorInfo=reactiveValues(strata=0,colors=c())
limit=reactiveValues(xmin=0,xmax=0)
pvalPos <- reactiveValues(x="",y="")
    plotkm <- eventReactive(input$go,{
        if(is.null(input$data)){ return(NULL)}
        data=read.csv(input$data$datapath)
        if(nchar(input$survObject)==0 || nchar(input$survFit)==0){
            stop("Check out the data and create a surv object and a corresponding survfit")
        }
        surv_object=eval(parse(text=input$survObject))
        #surv_object=Surv(data$DAY_START,data$DAY_END,!data$OUTCOME%in%c("TO_DEATH","FU"))
        fit=eval(parse(text=input$survFit))
        #formula=paste("surv_object~",input$strata,sep="")
        #fit=survfit(as.formula(formula),data=data)
        limit$xmax=max(fit$time)
        if(input$startPoint){
            limit$xmin=min(fit$time)
        }else{
            limit$xmin=0
        }
        colorInfo$strata = ifelse(length(fit$strata) > 0, length(fit$strata), 1)
        
        if(is.null(input$col1)){
            colorInfo$colors=rainbow_hcl(colorInfo$strata)
        }else{
            colorInfo$colors=sapply(1:colorInfo$strata,function(i) input[[paste("col",i,sep="")]])
        }
        
        pvals=data.frame(text=input$pvalue,x=pvalPos$x,y=pvalPos$y,stringsAsFactors=FALSE)
        namesOfStrata=unlist(strsplit(input$legendText,split=","))
        base=gg_KM(fit,title=input$title,legend=input$legend,
                   confinterval=input$confinterval,startPoint=input$startPoint,
                   background=input$background,ticks=input$axisTicks,xlabel = input$xlab,ylabel = input$ylab,colors=colorInfo$colors,pval = pvals,timeInYears=input$timeInYears,namesOfStrata = namesOfStrata)
        plot=base$plot
        plotobject=""
        if(input$n.risk==TRUE){
            plotobject=addrisk(base)
            plot=grid.draw(plotobject)
        }
        return(list("plot"=plot,"plotobject"=plotobject))
    })

    output$download <- downloadHandler(
        filename <- "ggKM.pdf",
        content <- function(file){
            nrTicks=switch(input$axisTicks,"1x"=1,"2x"=2,"4x"=4)
            if(input$n.risk==TRUE){
                pdf(file, width =7.2+(nrTicks-1)*1.5 , height = 4.8)
                grid.newpage()
                grid.draw(plotkm()$plotobject)
                dev.off()
            }else{
                pdf(file, width =7.2+(nrTicks-1)*1.5  , height = 4.8)
                print(plotkm()$plot)
                dev.off()
            }
        }
    )
    output$colors <- renderUI({
        if(colorInfo$strata==0) return(NULL)
        listOfColors=lapply(1:colorInfo$strata,function(i){
           inputID=paste("col",i,sep="")
           colourInput(inputID, paste("Select color nr.",i,sep=" "), value = colorInfo$colors[i],showColour = "background")
        })
        do.call(tagList, listOfColors)
    })
    
    output$kmplot <- renderPlot({
        grid.newpage()
        plotkm()
        
    })
    output$dataHead <- renderTable({
        if(is.null(input$data)){ return(NULL)}
        head(read.csv(input$data$datapath))
        
    })
    observeEvent(input$plot_click,{
        pvalPos$y=input$plot_click$y
        pvalPos$x=input$plot_click$x*(limit$xmax-limit$xmin)+limit$xmin
    })
    #output$debug <- renderPrint({
    #})
}

shinyApp(ui,server)
