#' @title Create Plot Level ESRI Polygon Shapefiles (.shp)
#'
#' @description Methods for constructing multipolygon ESRI shapefiles (.shp)
#' with individual polygons containing agricultural plot boundaries. Utilizes
#' AB line to rotate polygons to the appropriate geospatial direction of research
#' plots, allowing for simple overlay upon Unmanned Aerial System (UAS) or other
#' geospatial datasets (e.g. orthomosaics, point clouds, etc.) for feature extraction.
#' 
#' It is recommended to define working directory for output files prior to executing plotshpcreate.
#'
#' @param A Vector of UTM coordinates (Easting,Northing) of "A" point, which corresponds
#' to the bottom left corner of the first field plot.
#' Note: This point is where the function begins building polygons. This point is intended
#' to be the midpoint of alley at the front of the first plot and the midpoint of the
#' @param B Vector of UTM coordinates (Easting,Northing) of "B" point, which corresponds
#' to the top left corner of the field trial within the same row as the "A" point
#' @param infile Data frame containing seed preperation file and experimental design
#' (i.e. coordinates of plots within the design grid)
#' @param outfile Character assignment to define outputfile names. If left null, default
#' file names will be produced.
#' @param nrowplot Number of adjacent rows that constitute a plot.Setting this parameter
#' to greater than one allows for different polygon options. Default set to 1.
#' @param multirowind Logic parameter that indicates if adjacent plot rows should be
#' combined and treated as a single plot shapefile and unique idenfifier. Setting to false
#' will combine adjacent plots and construct a single polygon that encompasses
#' all adjacent rows within a plot. Default set to true.
#' @param rowspc Row (i.e. column) spacing of a single row. Default 2.5 feet in reference to
#' 30 inch row spacing.
#' @param rowbuf Distance removed from both sides of rowspc to create a buffer zone between
#' plots boundaries. Default is 0.1 feet.
#' @param rangespc Range (i.e. row) spacing of a single row. This dimesnsion referes to the total
#' plot length including half alley distance on either side of the plot. Default 25 feet.
#' @param rangebuf Distance removed from both sides of rangespc to create a buffer zone between
#' plots boundaries. Default is 2 feet in reference to four foot allies. Note: If alleys are
#' 4 feet rangebuf should be set to 2 feet to remove from both ends of the polygon.
#' @param plotsubset Defines how many adjacent rows should be excluded from the shapefile.
#' This parameter is useful for multirow plots where the central plots are of interest. If plotsubset=1,
#' the outer row will be removed from both sides of the plot.
#' @param field Character vector to indicate the trial the shapefile is being developed for.
#' Recommended format example: CS17-G2FE, which inicated location (CS), year (17), and trial (G2FE).
#' @param unit Character vector that the unit of measure for the polygon dimesnions. Default is "feet".
#' Units can also be input as meters using unit="meter".
#' @param SquarePlot Logic parameter to indicated if PDF file is desired for visualization of none rotated polygons.
#' @param RotatePlot Logic parameter to indicated if PDF file is desired for visualization of rotated polygons.
#' @import rgdal sp 
#' @export
#' @return NULL 
#' @note it is recommendeed to repeat unique Barcodes and Plot numbers if there are multirow plots (mrowplot>1) as
#' the plotshpcreatre function accounts for this redundance within the function.
#'
#' @examples
#'
#'
#' ### Creates shape file for each individual with the inclusion of multirow plot design
#' ### with nrowplot>1 and multirowind=T. Unique identifiers are subset by the location
#' ### of the adjacent plots reading from left to right in geographical space.
#' 
#' ### Set working directory to where you want the files to be saved
#' setwd("C://Temp")
#'
#' plotshpcreate(A=c(746239.817,3382052.264), #Point A c(Easting_0.0,Northing_0.0)
#'               B=c(746334.224,3382152.870), #Point B c(Easting_1.0,Northing_1.0)
#'               infile=SampleInfile,
#'               outfile="Ind_Multirow_plots",
#'               nrowplot=2,
#'               multirowind=T,
#'               rowspc=2.5,
#'               rowbuf=0.1, ### Will take off both sides of the plot
#'               rangespc=25,
#'               rangebuf=2, ### Half the alley width
#'               plotsubset=NULL,
#'               field="CS17-G2FE",
#'               unit="feet",
#'               SquarePlot=T,
#'               RotatePlot=T)
#'
#' # Creates shape file by combining adacent row of unique plots of multirow plot design
#' # within a single polygone with nrowplot>1 and multirowind=T. If a plot is two rows wide
#' # the shape file will encompass the plot as a whole.
#' 
#' ### Set working directory to where you want the files to be saved
#' setwd("C://Temp")
#' plotshpcreate(A=c(746239.817,3382052.264), #Point A c(Easting_0.0,Northing_0.0)
#'               B=c(746334.224,3382152.870), #Point B c(Easting_1.0,Northing_1.0)
#'               infile=SampleInfile,
#'               outfile="Multirowplotscombined",
#'               nrowplot=2,
#'               multirowind=F,
#'               rowspc=2.5,
#'               rowbuf=0.1, ### Will take off both sides of the plot
#'               rangespc=25,
#'               rangebuf=2, ### Half the alley width
#'               plotsubset=NULL,
#'               field="CS17-G2FE",
#'               unit="feet",
#'               SquarePlot=T,
#'               RotatePlot=T)
#'
#' # If the experiment is a single row plot design utilize nrowplot=1.


plotshpcreate<-function(A=NULL, #Point A c(Easting_0.0,Northing_0.0)
                        B=NULL, #Point B c(Easting_1.0,Northing_1.0)
                        infile=NULL,
                        outfile=NULL,
                        nrowplot=1,
                        multirowind=F,
                        rowspc=2.5,
                        rowbuf=0.1, ### Will take off both sides of the plot
                        rangespc=25,
                        rangebuf=2, ### Half the alley width
                        plotsubset=NULL,
                        field=NULL,
                        unit="feet",
                        SquarePlot=T,
                        RotatePlot=T){

  if (!requireNamespace("rgdal", quietly = TRUE)) {
    stop("Package \"rgdal\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  options(digits=12)

  infile<-infile[order(as.numeric(infile$Plot),as.numeric(infile$Row)),]

  nRange<- length(unique(infile$Range))
  nRow<- length(unique(infile$Row))

  nPlot<- nrow(infile) #nRange*nRow
  if (nPlot != (nRange*nRow))
  {warning("Length of file is not equal to number of plots: may cause errors, please remove blank rows at end of file and check row/range numbering")}

  # rm(FirstPlot,LastPlot)
  for (i in 1:nPlot){
    ### Find bottom left plot
    if (infile$Range[i]==min(infile$Range)&& infile$Row[i]==min(infile$Row)) {BLCornerPlot<-i}
    ### Find bottom right plot
    if (infile$Range[i]==min(infile$Range)&& infile$Row[i]==max(infile$Row)) {BRCornerPlot<-i}
    ### Find top right plot
    if (infile$Range[i]==max(infile$Range)&& infile$Row[i]==max(infile$Row)) {TRCornerPlot<-i}
    ### Find back left plot
    if (infile$Range[i]==max(infile$Range)&& infile$Row[i]==min(infile$Row)) {TLCornerPlot<-i}
    }

  ### Takes AB line diffences for delta calculation
  DeltaEasting<- B[[1]]-A[[1]]
  DeltaNorthing<- B[[2]]-A[[2]]

  # DeltaEasting<- Easting_1.0-Easting_0.0
  # DeltaNorthing<- Northing_1.0-Northing_0.0

  #Calculates theta in Radians
  DirectionTheta<- atan(abs(DeltaNorthing)/abs(DeltaEasting)) #Radians

### R circles rotate clockwise starting at [0,0] i.e right 90deg of circle
  if(DeltaNorthing>0 && DeltaEasting>0){
    Theta<-(((3*pi)/2)+DirectionTheta)
    srt_theta<-90-DirectionTheta*(180/pi)
    }
  if(DeltaNorthing<0 && DeltaEasting>0){
    Theta<-(((3*pi)/2)-DirectionTheta)
    srt_theta<-360-DirectionTheta*(180/pi)
    }
  if(DeltaNorthing<0 && DeltaEasting<0){
    Theta<-(((pi)/2)+DirectionTheta)
    srt_theta<-DirectionTheta*(180/pi)
  }
  if(DeltaNorthing>0 && DeltaEasting<0){
    Theta<-(((pi)/2)-DirectionTheta)
    srt_theta<-270+DirectionTheta*(180/pi)
    }

  if(unit=="feet"){
    RangeSpacingM<-rangespc/3.281 #meter
    RowSpacingM<- rowspc/3.281 #meter
    RangeBufferM<-rangebuf/3.281 #meter
    RowBufferM<- rowbuf/3.281 #meter
    }

  if(unit=="meter"){
    RangeSpacingM<-rangespc #meter
    RowSpacingM<- rowspc #meter
    RangeBufferM<-rangebuf #meter
    RowBufferM<- rowbuf #meter
    }


  ZeroZero<-matrix(0,1,2)
  ZeroZero[1,1] <- 0 #Easting_0.0
  ZeroZero[1,2] <- 0 #Northing_0.0

  ### When plots are perfectly perpendicular as if planted perfectly north to south
  PlotsSquareM<-matrix(NA,nPlot,11)
  PlotsSquareM[,1]<- infile$Plot # Plot number
  PlotsSquareM[,2]<- infile$Range-min(infile$Range)+1 #RANGE = Y
  PlotsSquareM[,3]<- infile$Row-min(infile$Row)+1 #ROW = X
  rownames(PlotsSquareM)<-rep(NA,nPlot)
  
  if(nrowplot==1){
    rownames(PlotsSquareM)<-infile$Barcode
  }
  # PlotsSquareM[,12]<- as.character(infile$Barcode) # Plot number

  ####################################################################
  ####################################################################
  ##### Combining Multi-row adjacent plots to a single plot ##########
  ####################################################################
  ####################################################################
  if(nrowplot>1 & multirowind==F){

    if(unit=="feet"){
      RangeSpacingM<-rangespc/3.281 #meter
      RowSpacingM<- (nrowplot*rowspc)/3.281 #meter
      RangeBufferM<-rangebuf/3.281 #meter
      RowBufferM<- rowbuf/3.281 #meter
    }

    if(unit=="meter"){
      RangeSpacingM<-rangespc #meter
      RowSpacingM<- nrowplot*rowspc #meter
      RangeBufferM<-rangebuf #meter
      RowBufferM<- rowbuf #meter
    }

    rowspc<-rowspc*nrowplot

    ID<-unique(as.character(infile$Barcode))

    for(i in 1:length(ID)){
      rownames(PlotsSquareM)[which(PlotsSquareM[,1]==infile$Plot[infile$Barcode==ID[[i]]])]<-ID[i]
    }

    PlotsSquareM<-PlotsSquareM[PlotsSquareM[,3]==1 | PlotsSquareM[,3] %in% seq(1+nrowplot,max(PlotsSquareM[,3]),nrowplot),]

    nPlot<-nrow(PlotsSquareM)

    for (i in 1:length(PlotsSquareM[,3])){
      if(PlotsSquareM[i,3]==1){next}
      else(PlotsSquareM[i,3]<-ceiling(PlotsSquareM[i,3]/nrowplot))
    }


    ########### Subseting out region of larger plot ################
    if(!is.null(plotsubset)){
      if(nrowplot==1){
        stop("nrowplot == 1 : Cannot subset singular plot.",
                       call. = FALSE)
        }
      if(nrowplot<3){
        stop("nrowplot < 3 : Cannot subset central plot. \n Recommend nrowplot=2, multirowind==T,plotsubset=NULL. ",
             call. = FALSE)
      }
      if(nrowplot==2*plotsubset){
        stop("nrowplot==2*plotsubset : no polygons will be created ",
             call. = FALSE)
      }

    }

      rowbuff<-rowbuf+(rowspc*plotsubset)
}

  ####################################################################
  ####################################################################
  ##### Formatting Adjacent Plot to Unique IDs w/o combining #########
  ####################################################################
  ####################################################################
  if(nrowplot>1 & multirowind==T){

    IDs<-unique(as.character(infile$Barcode))

  PolygonsToMake <- vector('list', length(IDs))

  for(i in 1:length(IDs)){
    # for (j in 1:nRow){
      subset_PSM<-PlotsSquareM[which(PlotsSquareM[,1]==infile$Plot[infile$Barcode==IDs[[i]]]),]

      # subset_PSM<-rbind(subset_PSM,subset_PSM)
      # subset_PSM[2,3]<-50
      nrow_subset<-nrow(subset_PSM)
      for (k in 1:nrow_subset){
        if(k==nrow_subset){

          rownames(PlotsSquareM)[which(PlotsSquareM[,1]==subset_PSM[1] &
                                         PlotsSquareM[,3]==subset_PSM[3])]<-paste(IDs[[i]],k,sep="_")

        }

        if(k<nrow_subset){
        rownames(PlotsSquareM)[which(PlotsSquareM[,1]==subset_PSM[subset_PSM[,3]==min(subset_PSM[,3]),1] &
                                       PlotsSquareM[,3]==subset_PSM[subset_PSM[,3]==min(subset_PSM[,3]),3])]<-paste(IDs[i],k,sep="_")

        subset_PSM<-subset_PSM[-which(subset_PSM[,3]==min(subset_PSM[,3])),]
        }

      }
  }

  if(!is.null(plotsubset)){
    if(nrowplot==1){
      stop("nrowplot == 1 : Cannot subset singular plot.",
           call. = FALSE)
    }
    if(nrowplot<3){
      stop("nrowplot < 3 : Cannot subset central plot. \n Recommend nrowplot=2, multirowind==T,plotsubset=NULL. ",
           call. = FALSE)
    }
    if(nrowplot==2*plotsubset){
      stop("nrowplot==2*plotsubset : no polygons will be created ",
           call. = FALSE)
    }

      PlotsSquareM<-PlotsSquareM[which(PlotsSquareM[,3] %in% seq(plotsubset+1,nrowplot-plotsubset,1)),]
  }
  }


  ####################################################################
  ####################################################################
  ################ Serpentine ordering of plots ######################
  ####################################################################
  ####################################################################
  for (i in 1:nRange){

    subset_PSM<-PlotsSquareM[PlotsSquareM[,2]==i,]

    if (i%%2==0){
      subset_PSM<-subset_PSM[order(subset_PSM[,3],decreasing=T),]
    }

    if (i%%2!=0){
      subset_PSM<-subset_PSM[order(subset_PSM[,3],decreasing=F),]
    }

    if(i==1){
      new_PSM<-subset_PSM
    }

    if(i!=1){
      new_PSM<-rbind(new_PSM,subset_PSM)
    }
  }

  PlotsSquareM<-new_PSM

  ####################################################################
  ####################################################################
  ####################################################################
  ####################################################################
  ### remember the point is in the middle of the plot not the furrow
  #### ^^^^^ I Think this is wrong -Steve 1/22/2019

  # i<-2
  for (i in 1:nPlot) {
    PlotsSquareM[i,4]<- ZeroZero[1,1]+((PlotsSquareM[i,2]-1)*RangeSpacingM)  # Y bottom left Range
    PlotsSquareM[i,5]<- ZeroZero[1,2]+((PlotsSquareM[i,3]-1)* RowSpacingM)  # X bottom left Row

    PlotsSquareM[i,6]<- ZeroZero[1,1]+((PlotsSquareM[i,2]-1)*RangeSpacingM )  # Y bottom right Range
    PlotsSquareM[i,7]<- ZeroZero[1,2]+((PlotsSquareM[i,3])* RowSpacingM)  # X bottom right Row

    PlotsSquareM[i,8]<- ZeroZero[1,1]+((PlotsSquareM[i,2])*RangeSpacingM )  # Y top right Range
    PlotsSquareM[i,9]<- ZeroZero[1,2]+((PlotsSquareM[i,3])* RowSpacingM)  # X top right Row

    PlotsSquareM[i,10]<- ZeroZero[1,1]+((PlotsSquareM[i,2])*RangeSpacingM )  # Y top left Range
    PlotsSquareM[i,11]<- ZeroZero[1,2]+((PlotsSquareM[i,3]-1)* RowSpacingM)  # X top left Row
  }

  #PlotsSquareM[c(BLCornerPlot, BRCornerPlot, TRCornerPlot, TLCornerPlot, FirstPlot, LastPlot),]

  ### THis is the red boxes
  ## has the data we care about
  PlotsSquareMBuf<-PlotsSquareM
  for (i in 1:nPlot) {
    PlotsSquareMBuf[i,4]<- ZeroZero[1,1]+((PlotsSquareMBuf[i,2]-1)*RangeSpacingM ) + RangeBufferM  # Y bottom left
    PlotsSquareMBuf[i,5]<- ZeroZero[1,2]+((PlotsSquareMBuf[i,3]-1)* RowSpacingM) + RowBufferM # X bottom left

    PlotsSquareMBuf[i,6]<- ZeroZero[1,1]+((PlotsSquareMBuf[i,2]-1)*RangeSpacingM ) +RangeBufferM # Y bottom right
    PlotsSquareMBuf[i,7]<- ZeroZero[1,2]+((PlotsSquareMBuf[i,3])* RowSpacingM) - RowBufferM  # X bottom right

    PlotsSquareMBuf[i,8]<- ZeroZero[1,1]+((PlotsSquareMBuf[i,2])*RangeSpacingM ) -RangeBufferM # Y top right
    PlotsSquareMBuf[i,9]<- ZeroZero[1,2]+((PlotsSquareMBuf[i,3])* RowSpacingM) - RowBufferM # X top right

    PlotsSquareMBuf[i,10]<- ZeroZero[1,1]+((PlotsSquareMBuf[i,2])*RangeSpacingM ) - RangeBufferM # Y top left
    PlotsSquareMBuf[i,11]<- ZeroZero[1,2]+((PlotsSquareMBuf[i,3]-1)* RowSpacingM) + RowBufferM # X top left
  }


  ###### PLOTTING SIZE OF PLOT DIMENSIONS ############
  if (SquarePlot==T){
  PlotY<- cbind(PlotsSquareM[,4], PlotsSquareM[,6], PlotsSquareM[,8], PlotsSquareM[,10]) #Ranges
  PlotX<- cbind(PlotsSquareM[,5], PlotsSquareM[,7], PlotsSquareM[,9], PlotsSquareM[,11]) #Rows outline

  PlotYBuf<- cbind(PlotsSquareMBuf[,4], PlotsSquareMBuf[,6], PlotsSquareMBuf[,8], PlotsSquareMBuf[,10]) #Ranges buffered
  PlotXBuf<- cbind(PlotsSquareMBuf[,5], PlotsSquareMBuf[,7], PlotsSquareMBuf[,9], PlotsSquareMBuf[,11]) #Ranges buffered


  ########## Square Visualization of Trial Plots ##############
  pdf(paste(field,outfile,"Square_plots.pdf",sep="_"),
      width=15,
      height=15)
  plot(c(min(PlotX),max(PlotX)), c(min(PlotY),max(PlotY)), type="n", xlab = "ROWS (meters)", ylab = "RANGES (meters)") # Plot lines in field

  for (i in 1:nPlot) {
    for (j in 1:4) {
      points(PlotX[i,j], PlotY[i,j])
    }
    polygon(PlotX[i,], PlotY[i,])
    polygon(PlotXBuf[i,], PlotYBuf[i,], col="RED")
    text( ((PlotXBuf[i,1]+PlotXBuf[i,3])/2), ((PlotYBuf[i,1]+PlotYBuf[i,3])/2), labels= PlotsSquareM[i,1], pos = NULL, col = "WHITE", cex=0.7,srt=90 )
  }
  dev.off()
  }


  ########## ADJUST PLOT CORNERS TO NOT CENTER OF PLOT ##############

  ThetaPlotCorner<-Theta
  NorthingCorner_0.0<-A[2]
  EastingCorner_0.0<-A[1]
  PlotsAdjustedM<- PlotsSquareM
  PlotsAdjustedMBuf<- PlotsSquareMBuf


  ####################################################################
  ####################################################################
  ######## Rotating Plot Boundaries Based on AB Line #################
  ####################################################################
  ####################################################################
  for (i in 1:nPlot) {

    #BottomLeft
    PlotsAdjustedM[i,4]<- (PlotsSquareM[i,4]*cos(Theta))+ (PlotsSquareM[i,5]*sin(Theta)) + NorthingCorner_0.0  # Y bottom left ranges
    PlotsAdjustedM[i,5]<- (PlotsSquareM[i,5]*cos(Theta))- (PlotsSquareM[i,4]*sin(Theta)) + EastingCorner_0.0  # X bottom left rows

    #BottomRight
    PlotsAdjustedM[i,6]<- (PlotsSquareM[i,6]*cos(Theta))+ (PlotsSquareM[i,7]*sin(Theta)) + NorthingCorner_0.0   # Y bottom right ranges
    PlotsAdjustedM[i,7]<- (PlotsSquareM[i,7]*cos(Theta))- (PlotsSquareM[i,6]*sin(Theta)) + EastingCorner_0.0  # X bottom right rows

    #TopRight
    PlotsAdjustedM[i,8]<- (PlotsSquareM[i,8]*cos(Theta))+ (PlotsSquareM[i,9]*sin(Theta)) + NorthingCorner_0.0   # Y top right ranges
    PlotsAdjustedM[i,9]<- (PlotsSquareM[i,9]*cos(Theta))- (PlotsSquareM[i,8]*sin(Theta)) + EastingCorner_0.0  # X top right rows

    #TopLeft
    PlotsAdjustedM[i,10]<- (PlotsSquareM[i,10]*cos(Theta))+ (PlotsSquareM[i,11]*sin(Theta)) + NorthingCorner_0.0   # Y top left ranges
    PlotsAdjustedM[i,11]<- (PlotsSquareM[i,11]*cos(Theta))- (PlotsSquareM[i,10]*sin(Theta)) + EastingCorner_0.0  # X top left rows

    ##BottomLeft Adjusted Buffer
    PlotsAdjustedMBuf[i,4]<- (PlotsSquareMBuf[i,4]*cos(Theta))+ (PlotsSquareMBuf[i,5]*sin(Theta)) + NorthingCorner_0.0 # Y bottom left ranges
    PlotsAdjustedMBuf[i,5]<- (PlotsSquareMBuf[i,5]*cos(Theta))- (PlotsSquareMBuf[i,4]*sin(Theta)) + EastingCorner_0.0  # X bottom left rows

    #BottomRight Adjusted Buffer
    PlotsAdjustedMBuf[i,6]<- (PlotsSquareMBuf[i,6]*cos(Theta))+ (PlotsSquareMBuf[i,7]*sin(Theta)) + NorthingCorner_0.0# Y bottom right ranges
    PlotsAdjustedMBuf[i,7]<- (PlotsSquareMBuf[i,7]*cos(Theta))- (PlotsSquareMBuf[i,6]*sin(Theta)) + EastingCorner_0.0 # X bottom right rows

    #TopRight Adjusted Buffer
    PlotsAdjustedMBuf[i,8]<- (PlotsSquareMBuf[i,8]*cos(Theta))+ (PlotsSquareMBuf[i,9]*sin(Theta)) + NorthingCorner_0.0# Y top right ranges
    PlotsAdjustedMBuf[i,9]<- (PlotsSquareMBuf[i,9]*cos(Theta))- (PlotsSquareMBuf[i,8]*sin(Theta)) + EastingCorner_0.0  # X top right rows

    #TopLeft Adjusted Buffer
    PlotsAdjustedMBuf[i,10]<- (PlotsSquareMBuf[i,10]*cos(Theta))+ (PlotsSquareMBuf[i,11]*sin(Theta)) + NorthingCorner_0.0# Y top left ranges
    PlotsAdjustedMBuf[i,11]<- (PlotsSquareMBuf[i,11]*cos(Theta))- (PlotsSquareMBuf[i,10]*sin(Theta)) + EastingCorner_0.0  # X top left rows

  }
  names(PlotsAdjustedM) <- c( "Plot", "Range", "Row", "YBL", "XBL", "YBR", "XBR", "YTR", "XTR", "YTL", "XTL")
  names(PlotsAdjustedMBuf) <- c( "Plot", "Range", "Row", "YBLbuf", "XBLbuf", "YBRbuf", "XBRbuf", "YTRbuf", "XTRbuf", "YTLbuf", "XTLbuf")


  ###### PLOTTING SIZE OF PLOT DIMENSIONS ############
  if (RotatePlot==T){
  PlotY<- cbind(PlotsAdjustedM[,4], PlotsAdjustedM[,6], PlotsAdjustedM[,8], PlotsAdjustedM[,10])
  PlotX<- cbind(PlotsAdjustedM[,5], PlotsAdjustedM[,7], PlotsAdjustedM[,9], PlotsAdjustedM[,11])

  ###### PLOTTING SIZE OF PLOT DIMENSIONS ############
  PlotYBuf<- cbind(PlotsAdjustedMBuf[,4], PlotsAdjustedMBuf[,6], PlotsAdjustedMBuf[,8], PlotsAdjustedMBuf[,10])
  PlotXBuf<- cbind(PlotsAdjustedMBuf[,5], PlotsAdjustedMBuf[,7], PlotsAdjustedMBuf[,9], PlotsAdjustedMBuf[,11])

  ########## PLOTTING ##############
  pdf(paste(field,outfile,"Rotated_plots.pdf",sep="_"),
      width=30,
      height=30)
  plot(c(min(PlotX),max(PlotX)), c(min(PlotY),max(PlotY)), type="n", xlab = "ROWS", ylab = "RANGES") # Plot lines in field

  for (i in 1:nPlot) {
    for (j in 1:4) {
      points(PlotX[i,j], PlotY[i,j])
    }
    polygon(PlotX[i,], PlotY[i,])
    polygon(PlotXBuf[i,], PlotYBuf[i,], col="RED")
    text( ((PlotX[i,1]+PlotX[i,3])/2), ((PlotY[i,1]+PlotY[i,3])/2), labels= PlotsSquareM[i,1], pos = NULL, col = "BLUE", cex=0.7, srt=srt_theta)
  }
  dev.off()
  }


 ###################################################################
#   ########## Make Shape Files4 ##############
# if(multirowind!=T){

  # if (file.exists(paste(output,"\\",field,".shp",sep="")) |
  #     file.exists(paste(output,"\\",field,".dbf",sep="")) |
  #     file.exists(paste(output,"\\",field,".shx",sep="")))
  #   {
  #   file.remove(paste(output,"\\",field,".shp",sep=""))
  #   file.remove(paste(output,"\\",field,".dbf",sep=""))
  #   file.remove(paste(output,"\\",field,".shx",sep=""))
  #   }


  PolygonsToMake <- vector('list', length(nPlot))
  for (i in 1: nPlot) {
    PolygonsToMake[[i]] <- Polygons(list(Polygon(matrix(c(PlotsAdjustedM[i,c(5,7,9,11,5)],
                                                          PlotsAdjustedM[i,c(4,6,8,10,4)]),5,2),
                                                 hole=FALSE)),
                                    rownames(PlotsAdjustedM)[i])
  }

  SpatialPolygonsToMake<- SpatialPolygons((PolygonsToMake))

  SpatialPolygonsToMake.df <- SpatialPolygonsDataFrame(SpatialPolygonsToMake,
                                                       data.frame(id=rownames(PlotsAdjustedM),
                                                                  row.names=rownames(PlotsAdjustedM)))


  FolderpathGPSPlots<-paste(field,"_",outfile,".shp",sep="")
  writeOGR(SpatialPolygonsToMake.df,
           FolderpathGPSPlots,
           field,
           verbose = TRUE,
           overwrite_layer = T,
           driver="ESRI Shapefile")


  ########## Make Shape Files4 ##############
  # ord_infile<-infile[order(infile$Range,infile$Row)]

  # IDs<- as.character(infile$Barcode)

  PolygonsToMake <- vector('list', length(nPlot))
  for (i in 1: nPlot) {
    PolygonsToMake[[i]] <- Polygons(list(Polygon(matrix(c(PlotsAdjustedMBuf[i,c(5,7,9,11,5)],
                                                          PlotsAdjustedMBuf[i,c(4,6,8,10,4)]),5,2), hole=FALSE)),
                                    rownames(PlotsAdjustedMBuf)[i])
  }

  SpatialPolygonsToMake<- SpatialPolygons((PolygonsToMake))

  SpatialPolygonsToMake.df <- SpatialPolygonsDataFrame(SpatialPolygonsToMake,
                                                       data.frame(id=rownames(PlotsAdjustedMBuf),
                                                                  row.names=rownames(PlotsAdjustedMBuf)))


  FolderpathGPSPlots<-paste(field,outfile,"buff.shp",sep="_")
  writeOGR(SpatialPolygonsToMake.df,
           FolderpathGPSPlots,
           paste(field,"Buff",sep="_"),
           verbose = TRUE,
           overwrite_layer = T,
           driver="ESRI Shapefile")
}
  # }
