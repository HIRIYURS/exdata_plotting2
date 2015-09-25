## Project 2 - Question 1

plot1 <- function() {
    require(dplyr)
    require(data.table)
    
    neifile <- "./exdata/exdata-data-NEI_data/summarySCC_PM25.rds"
    sccfile <- "./exdata/exdata-data-NEI_data/Source_Classification_Code.rds"
    ## This first line will likely take a few seconds. Be patient!
    NEI <- readRDS(neifile)
    SCC <- readRDS(sccfile)
    
    # Convert to a table
    neitbl <- tbl_df(NEI)
    
    # Group by year
    emdata <- group_by(neitbl, year)

    # Sum up the emmissions
    sumdata <- summarize(emdata, sum(Emissions))

    # Assign the colnames
    colnames(sumdata) <- c("year", "Emissions")
    
    # Plot the graph on to a png file
    png("./exdata/plot1.png", 720, 480)
    
    # If you want line graph
    #plot(sumdata, type="l")
    
    # Draw barplot (Convert to (Thousand Tons) K Tons = div by 10^3)
    barplot(sumdata$Emissions/10^3,
        names.arg = sumdata$year,
        xlab = "Year",
        ylab = "Emissions PM2.5 (K Tons)",
        main = "Total Emissions (PM2.5) From All Sources across USA",
        col = "blue")
    
    # Turn off the device to write the file on to disk
    dev.off()
    
    print("complete")
}

## Project 2 - Question 2

plot2 <- function() {
    require(dplyr)
    require(data.table)
    
    neifile <- "./exdata/exdata-data-NEI_data/summarySCC_PM25.rds"
    sccfile <- "./exdata/exdata-data-NEI_data/Source_Classification_Code.rds"

    ## This first line will likely take a few seconds. Be patient!
    NEI <- readRDS(neifile)
    SCC <- readRDS(sccfile)
    
    # Create the table, group by year and then sum up the emmissions
    neitbl <- tbl_df(NEI)
    
    # Filter for Baltimore city, fips = "24510"
    baltidata <- filter(neitbl, fips=="24510")
    
    # Group by Year
    emdata <- group_by(baltidata, year)
    
    # Sum up the Emissions
    sumdata <- summarize(emdata, sum(Emissions))

    # Assign the colnames
    colnames(sumdata) <- c("year", "Emissions")
    
    # Plot the graph on to a png file
    png("./exdata/plot2.png", 720, 480)

    # Plot Line graph if you want
    #plot(sumdata, type="l")

    # Draw barplot
    barplot(sumdata$Emissions,
        names.arg = sumdata$year,
        xlab = "Year",
        ylab = "Emissions PM2.5 (Tons)",
        main = "Total Emissions (PM2.5) From All Sources for Baltimore City",
        col = "red")
    
    # Turn off the device to write the file on to disk
    dev.off()
    
    print("complete")
}

plot3 <- function() {
    require(dplyr)
    require(data.table)
    require(ggplot2)
    
    neifile <- "./exdata/exdata-data-NEI_data/summarySCC_PM25.rds"
    sccfile <- "./exdata/exdata-data-NEI_data/Source_Classification_Code.rds"
    
    ## This first line will likely take a few seconds. Be patient!
    NEI <- readRDS(neifile)
    SCC <- readRDS(sccfile)
    
    # Convert it into a table
    neitbl <- tbl_df(NEI)
    
    # Filter by Baltimore city fips = 24510 
    baltidata <- filter(neitbl, fips=="24510")
    
    # Group by year and type
    emdata <- group_by(baltidata, year, type)
    
    # Summarize by summing up the emissions based on types of sources
    sumdata <- summarize(emdata, sum(Emissions))
    
    # Assign the colnames
    colnames(sumdata) <- c("year", "type", "Emissions")
    
    # Plot the graph on to a png file, pixels 720x480
    png("./exdata/plot3.png", 720, 480)
    
    # Plot Barplot using ggplot
    gplot3 <- ggplot(sumdata, aes(factor(year),Emissions,fill=type)) +
        geom_bar(stat="identity") +
        facet_grid(.~type,scales = "free",space="free") + 
        labs(x = "Year", 
             y = expression("Total PM"[2.5]*" Emission (Tons)")) + 
        labs(title = expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Types of Sources"))
    
    print(gplot3)
    
    dev.off()
    
    # Less Fancy - Print with qplot function
    #qplot(year, Emissions, data = baltidata, 
    #      facets = .~type, geom=c("histogram"), stat="identity",
    #      xlab = "Year",
    #      ylab = expression("Total PM"[2.5]*" Emission (Tons)"),
    #      title = expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Types of Sources"))

    print("complete")
}

plot4 <- function() {
    require(dplyr)
    require(data.table)
    require(ggplot2)
    
    neifile <- "./exdata/exdata-data-NEI_data/summarySCC_PM25.rds"
    sccfile <- "./exdata/exdata-data-NEI_data/Source_Classification_Code.rds"
    
    ## This first line will likely take a few seconds. Be patient!
    NEI <- readRDS(neifile)
    SCC <- readRDS(sccfile)
    
    # Convert it into a table
    neitbl <- tbl_df(NEI)
    scctbl <- tbl_df(SCC)
    
    # Filter Rows in SCC table based on Column EI.Sector with value Comb*Coal
    coalcomb <- filter(scctbl, grepl("Fuel Comb.*Coal", EI.Sector),
                                    ignore.case = TRUE)
    
    # Match the rows in neitbl with coalcomb
    neicoalcomb <- filter(neitbl, SCC %in% coalcomb$SCC)
    
    # Group by Year
    emdata <- group_by(neicoalcomb, year)
    
    # Sum up Emmissions
    sumdata <- summarize(emdata, sum(Emissions))
    
    # Assign colnames
    colnames(sumdata) <- c("year", "Emissions")
 
    # Plot the graph on to a png file, pixels 720x480
    png("./exdata/plot4.png", 720, 480)
    
    # Draw the bar plot (in terms of K Tons, 10^3)
    #gplot4 <- ggplot(sumdata,aes(factor(year),Emissions/10^3)) +
    #    geom_bar(stat="identity",fill="black",width=0.75) +
    #    theme_bw() +  guides(fill=FALSE) +
    #    labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
    #    labs(title=expression("PM"[2.5]*" Motor Vehicle based Emissions in Baltimore City from 1999-2008"))    
    
    #print(gplot4)
    
    barplot(sumdata$Emissions/10^3,
           names.arg = sumdata$year,
            xlab = "Year",
            ylab = "Emissions PM2.5 (K Tons)",
            main = "Total Emissions (PM2.5) From Coal Combustion across USA",
            col = "grey")
    
    # Close the device
    dev.off()
    
    print("Complete")
}

plot5 <- function() {
    require(dplyr)
    require(data.table)
    require(ggplot2)
    
    neifile <- "./exdata/exdata-data-NEI_data/summarySCC_PM25.rds"
    sccfile <- "./exdata/exdata-data-NEI_data/Source_Classification_Code.rds"
    
    ## This first line will likely take a few seconds. Be patient!
    NEI <- readRDS(neifile)
    SCC <- readRDS(sccfile)
    
    # Convert it into a table
    neitbl <- tbl_df(NEI)
    scctbl <- tbl_df(SCC)
    
    # Filter by Baltimore city fips = 24510 
    baltidata <- filter(neitbl, fips=="24510")
    
    # Find Emmissions caused by Motor vehicles
    vehiclescc <- filter(scctbl, grepl("Vehicle", 
                                   SCC.Level.Two, ignore.case=TRUE))
     
    # Match the rows in baltidata with vehiclescc
    vehiclenei <- filter(baltidata, SCC %in% vehiclescc$SCC)
    
    # Group by year and type
    emdata <- group_by(vehiclenei, year, type)
    
    # Summarize by summing up the emissions based on types of sources
    sumdata <- summarize(emdata, sum(Emissions))
    
    # Assign the colnames
    colnames(sumdata) <- c("year", "type", "Emissions")
    
    # Plot the graph on to a png file, pixels 720x480
    png("./exdata/plot5.png", 720, 480)
    
    # Plot Barplot using ggplot
    gplot5 <- ggplot(sumdata,aes(factor(year),Emissions)) +
        geom_bar(stat="identity",fill="green",width=0.75) +
        theme_bw() +  guides(fill=FALSE) +
        labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
        labs(title=expression("PM"[2.5]*" Motor Vehicle based Emissions in Baltimore City from 1999-2008"))    

    print(gplot5)
    
    dev.off()
    
    print("complete")
}

plot6 <- function() {
    require(dplyr)
    require(data.table)
    require(ggplot2)
    
    neifile <- "./exdata/exdata-data-NEI_data/summarySCC_PM25.rds"
    sccfile <- "./exdata/exdata-data-NEI_data/Source_Classification_Code.rds"
    
    ## This first line will likely take a few seconds. Be patient!
    NEI <- readRDS(neifile)
    SCC <- readRDS(sccfile)
    
    # Convert it into a table
    neitbl <- tbl_df(NEI)
    scctbl <- tbl_df(SCC)
    
    # Filter by Baltimore city fips = 24510 and LA City fips = 24510 
    baltidata <- filter(neitbl, fips == "24510")

    # Filter by LA city fips = 06037 
    ladata <- filter(neitbl, fips=="06037")
 
    # Find Emmissions caused by Motor vehicles
    vehiclescc <- filter(scctbl, grepl("Vehicle", 
                                       SCC.Level.Two, ignore.case=TRUE))

    # Match the rows in baltidata with vehiclescc
    baltivehicle <- filter(baltidata, SCC %in% vehiclescc$SCC)

    # Match the rows in ladata with vehiclescc
    lavehicle <- filter(ladata, SCC %in% vehiclescc$SCC)
    
    # Group by year and type for Baltimore
    baltiemi <- group_by(baltivehicle, year, type)
    
    # Summarize by summing up the emissions based on types of sources
    baltisumdata <- summarize(baltiemi, sum(Emissions))
    
    # Group by year and type for LA
    laemi <- group_by(lavehicle, year, type)
    
    # Summarize by summing up the emissions based on types of sources
    lasumdata <- summarize(laemi, sum(Emissions))
    
    # Assign the colnames
    colnames(baltisumdata) <- c("year", "type", "Emissions")
    colnames(lasumdata) <- c("year", "type", "Emissions")
    
    # Add city name to the new tables
    baltisumdata$City <- "Baltimore"
    lasumdata$City <- "Los Angeles"
    
    # Row bind the two tables for Baltimore and LA
    balamvdata <- rbind(baltisumdata, lasumdata)

    # Plot the graph on to a png file, pixels 720x480
    png("./exdata/plot6.png", 720, 480)
    
    # Draw the plot using ggplot
    gplot6 <- ggplot(balamvdata, aes(factor(year), Emissions, fill=City)) +
        geom_bar(stat="identity") +
        facet_grid(scales="free", space="free", . ~ City) +
        labs(x="Year", 
             y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
        labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))
    
    print(gplot6)
    
    # Close the device
    dev.off()
    print("Complete")
}
