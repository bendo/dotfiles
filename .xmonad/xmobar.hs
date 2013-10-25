Config {
    font = "xft:Fixed-8",
    bgColor = "#3c3f41",
    fgColor = "#eeeeee",
    position = Static { xpos = 0, ypos = 0, width = 1366, height = 16 },
    lowerOnStart = True,
    commands = [
        Run Weather "EDDM" ["-t","<tempC>C <skyCondition>","-L","64","-H","77","-n","#5BCE2D","-h","#FF2424","-l","#96CBFE"] 36000,
        Run MultiCpu ["-t","CPU: <total0> <total1> <total2> <total3> <total4> <total5> <total6> <total7>","-L","30","-H","60","-h","#FF2424","-l","#5BCE2D","-n","#FFFC24","-w","3"] 10,
        Run Memory ["-t","MEM: <usedratio>%","-H","8192","-L","4096","-h","#FF2424","-l","#5BCE2D","-n","#FFFC24"] 10,
        Run Network "eth0" ["-t","NET: <rx>, <tx>","-H","200","-L","10","-h","#FF2424","-l","#5BCE2D","-n","#FFFC24"] 10,
	Run Uptime ["-t","<days>d <hours>h <minutes>m"] 10,
        Run Date "%_d %b" "date" 10,
        Run Date "%H:%M:%S" "time" 10,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %memory% <fc=#ff6f00>    %date%    %time%</fc>"
}
