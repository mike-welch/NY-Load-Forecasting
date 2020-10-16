
## Get API Key
$API = Get-Content -Path '.\MESOWEST_API.json' | ConvertFrom-Json


function Get-APIData{
param(
    [string]$ID,
    [string]$OutFile,
    [int]$Year,
    [string]$API_Root = 'http://api.mesowest.net/v2',
    [string]$API_Service = 'stations/timeseries?'
)

function Add-APIvariable{
    param( [string]$Name )
    [string]$output = $args | ForEach-Object -Begin {"&$Name="} -Process {"$_,"}
    $output.TrimEnd(',') -replace '\s+','' -replace '~',' '
}


[string]$uri_root = "$API_Root\$API_Service"
$uri_root += Add-APIvariable -Name output csv
$uri_root += Add-APIvariable -Name stid $ID
$uri_root += Add-APIvariable -Name token $API.Token
$uri_root += Add-APIvariable -Name vars altimeter air_temp air_temp_wet_bulb relative_humidity dew_point_temperature wind_speed qc
$uri_root += Add-APIvariable -Name obtimezone UTC
$uri_root += Add-APIvariable -Name units 'temp|F' 'speed|mph' 'pres|pa' 'height|m' 'precip|cm' 'alti|pa'
$uri_root += Add-APIvariable -Name timeformat '%Y-%m-%d~%H:%M:%S'


$date = Get-Date -Year $year -Month 1 -Day 1 -Hour 0 -Minute 0 -Second 0

New-Item -Path $year -ItemType directory | Push-Location
$start = '0000'
$end = '2300'
do{
    [string]$yyyyMMdd = $date.ToString('yyyyMMdd')

    $uri = $uri_root
    $uri += Add-APIvariable -Name start $date.ToString('yyyyMMddHHmm')
    $uri += Add-APIvariable -Name end $date.AddDays(1).ToString('yyyyMMddHHmm')

    Invoke-RestMethod -Method Get -Uri "$uri" -OutFile "$ID`_$yyyyMMdd.csv"

    $date = $date.AddDays(1)
}while( $date.Year -eq $Year )

Pop-Location

Write-host "Merging files... " -nonewline

$storeCSV = "$year-$ID.csv"
$paths = Get-ChildItem -Path "$year\*.csv" | Sort-Object
Get-Content -path $paths[0] | Select-Object -First 8 | Set-Content -Path $storeCSV
foreach( $file in $paths ){
    Get-Content $file | Select-Object -Skip 8  | Add-Content -Path $storeCSV
}
Remove-Item $year -Recurse
Write-Host "Done!`n"

}


[string[]]$stations =  @('KBUF','KROC','KSYR','KMSS','KBGM','KALB','KPOU','KHPN','KLGA','KJFK','KHWV', 'KISP')
[int[]]$years = 2016

foreach( $station in $stations ){

    foreach( $year in $years ){
        Get-APIData -ID $station -Year $year
    }
}
