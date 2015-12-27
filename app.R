library(ggKaplanMeyer)
library(survival)
library(gtable)
library(ggplot2)
library(shinyjs)
library(colorspace)
library(Cairo)
ui <- shinyUI(fluidPage(

    titlePanel("Kaplan-Meier with ggplot2"),
    fluidRow(
        column(4,
               wellPanel(
                fileInput('data',label="Choose data on csv format"),
                radioButtons("strata", label = "Strata",
                                   choices=list("Sex"="SEX","Current Smoker"="CURRENT_SMOKER","Ever a smoker"="EVER_SMOKER",Statins="STATINS","Overall"=1)),


                checkboxInput("actualAge", label="Time in research is the actual age of participants", value=FALSE),
                checkboxInput("n.risk", label="Add number at risk table", value=FALSE),
                checkboxInput("Advanced","Advanced settings",value=FALSE),
                conditionalPanel(condition="input.Advanced==true",
                h4("Plot properties"),
                uiOutput('colors'),
                radioButtons("axisTicks", label = "Nr. of axis ticks",
                             choices=list("1x"="1x","2x"="2x","4x"="4x")),
                checkboxInput("background",label="Background"),
                checkboxInput("confinterval", label="Include 95% confidence interval", value=TRUE),
               textInput('title',label="Title"),
               textInput('xlab',label="x-label"),
               textInput('ylab',label="y-label"),
               textInput('pvalue',label="p-value"),
                radioButtons("legend", label = "Legend position",
                             choices=list("None"="none","Bottom left"="bottom-left","Top left"="top-left","Top right"="top-right","Bottom right"="bottom-right"))
                ),

               
                actionButton("go",label="Submit"),
               downloadButton('download',label="Download plot as pdf")
               )
        ),
    column(8,

        plotOutput("kmplot",click = "plot_click"),         
        textOutput("debug")
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
        surv_object=Surv(data$DAY_START,data$DAY_END,!data$OUTCOME%in%c("TO_DEATH","FU"))
        formula=paste("surv_object~",input$strata,sep="")
        fit=survfit(as.formula(formula),data=data)
        limit$xmax=max(fit$time)
        if(input$actualAge){
            limit$xmin=min(fit$time)
        }else{
            limit$xmin=0
        }
        colorInfo$strata=length(fit$strata)
        
        if(is.null(input$col1)){
            colorInfo$colors=rainbow_hcl(colorInfo$strata)
        }else{
            colorInfo$colors=sapply(1:colorInfo$strata,function(i) input[[paste("col",i,sep="")]])
        }
        
        pvals=data.frame(text=input$pvalue,x=pvalPos$x,y=pvalPos$y,stringsAsFactors=FALSE)
        base=gg_KM(fit,title=input$title,legend=input$legend,
                   confinterval=input$confinterval,actualAge=input$actualAge,
                   background=input$background,ticks=input$axisTicks,xlabel = input$xlab,ylabel = input$ylab,colors=colorInfo$colors,pval = pvals)
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
            if(input$n.risk==TRUE){
                pdf(file, width =7.2 , height = 4.8)
                #jpeg(file, width =720 , height = 480) # Open a new png file
                grid.newpage()
                grid.draw(plotkm()$plotobject)
                dev.off()
            }else{
                pdf(file, width =7.2 , height = 4.8)
                #jpeg(file, width =720 , height = 480)
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
                
                #device <- function(..., width, height) grDevices::png(..., width =600 , height = 480, res = 300, units = "in")
                #ggsave(file, plot = plotkm()$plot, device = device)

    output$kmplot <- renderPlot({
        grid.newpage()
        plotkm()
        
    })
    observeEvent(input$plot_click,{
        pvalPos$y=input$plot_click$y
        pvalPos$x=input$plot_click$x*(limit$xmax-limit$xmin)+limit$xmin
    })
    output$debug <- renderPrint({

        pvals=data.frame(text=input$pvalue,x=pvalPos$x,y=pvalPos$y)
#         gg_KM(fit,title=input$title,legend=input$legend,
#               confinterval=input$confinterval,actualAge=input$actualAge,
#               background=input$background,ticks=input$axisTicks,xlabel = input$xlab,ylabel = input$ylab,colors=colorInfo$colors,pval = pvals)
       colorInfo$colors
       is.null(input$col1)
    })
}

shinyApp(ui,server)
