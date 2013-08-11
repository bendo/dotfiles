Config {
    font = "xft:Fixed-8",
    bgColor = "#000000",
    fgColor = "#ffffff",
    position = Static { xpos = 0, ypos = 0, width = 1366, height = 16 },
    lowerOnStart = True,
    commands = [
        Run Weather "MUC" ["-t","<tempF>F <skyCondition>","-L","64","-H","77","-n","#4e9a06","-h","#FFB6B0","-l","#96CBFE"] 36000,
        Run MultiCpu ["-t","Cpu: <total0> <total1>","-L","30","-H","60","-h","#FFB6B0","-l","#4e9a06","-n","#FFFFCC","-w","3"] 10,
        Run Memory ["-t","Mem: <usedratio>%","-H","8192","-L","4096","-h","#FFB6B0","-l","#4e9a06","-n","#FFFFCC"] 10,
        Run Network "eth0" ["-t","Net: <rx>, <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#4e9a06","-n","#FFFFCC"] 10,
        Run Date "%a %b %_d %l:%M" "date" 10,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %multicpu%  %memory%  %eth0%  <fc=#3465a4>%date%</fc>  %MUC%"
}
