
<#
 damlbmp_zone
 rtlbmp_zone
 isolf
 palIntegrated
#>


[int[]]$years = 2016
[string]$ID = 'P-7'


# P-2A : Day-Ahead Market LBMP, Zonal
# P-2B : Day-Ahead Market LBMP, Generator

# P-24A: Real-Time Market LBMP, Zonal
# P-24B: Real-Time Market LBMP, Generator

# P-4A : Time-Weighted/Integrated Real-Time LBMP, Zonal
# P-4B : Time-Weighted/Integrated Real-Time LBMP, Generator



###################################


[string]$downloadpath = $PSScriptRoot

Set-Location $downloadpath

[string]$Username = ''
[string]$Password = ''

$WebClient = New-Object System.Net.WebClient
$WebClient.Credentials = New-Object System.Net.NetworkCredential($Username,$Password)

Add-Type -AssemblyName System.IO.Compression.FileSystem


$First = @{}
switch( $ID.Split('-') | Select-Object -Last 1 ){
     '2A' { $name = 'damlbmp_zone'  }
     '2B' { $name = 'damlbmp_gen'   }
    '24A' { $name = 'realtime_zone' }
    '24B' { $name = 'realtime_gen'  }
     '4A' { $name = 'rtlbmp_zone'   }
     '4B' { $name = 'rtlbmp_gen'    }

     '7'  { $name = 'isolf'; $First.Add('First',24)}
     '7A' { $name = 'lfweather'  }
    '58B' { $name = 'pal'           }
    '58C' { $name = 'palIntegrated' }
    '59'  { $name = 'zonalBidLoad'  }

}


# $7zip = 'C:\Program Files\7-Zip\7zG.exe'
foreach( $year in $years ){

    New-Item -Path $year -ItemType directory | Push-Location


    for( $m = 1; $m -le 12; $m++ ){

        $mm = $m.ToString("00")
        $dd = '01'


        $filename = "$year$mm$dd$name`_csv"

        [string]$URL = "http://mis.nyiso.com/public/csv/" + ( $name.Split('_') | Select-Object -First 1) + '/' + $filename + '.zip'
        [string]$path = "$downloadpath/$year/$filename.zip"

        write-host "Importing $year-$mm... " -NoNewline
        $WebClient.DownloadFile( $URL, $path )

        [System.IO.Compression.ZipFile]::ExtractToDirectory($path, ($path | Split-Path -Parent) )

        Write-Host "Done!"
    }

    Get-ChildItem -Path *.zip | Remove-Item -Force

    $files = Get-ChildItem -Path *.csv | Sort-Object -Property FullName | Select-Object -ExpandProperty FullName
    Pop-Location

    Write-host "Merging files... " -nonewline

    $storeCSV = "$year-$name.csv"
    Get-Content -path $files[0] | Select-Object -First 1 | Set-Content -Path $storeCSV
    foreach( $file in $files ){
        [string[]]$data = Get-Content $file | Select-Object -Skip 1 @First


        if( $ID -eq 'P-7A' ){
            [int]$mo = ($file | Split-Path -Leaf).Substring(4,2)
            [int]$dy = ($file | Split-Path -Leaf).Substring(6,2)

            if( ($mo -eq 1) -and ($dy -le 3) ){
                $data = $data | Select-Object -First 17 -Skip (17*($dy-1))
            }elseif( ($mo -eq 12) -and ($dy -eq 31) ){
                $data = $data | Select-Object -First 17 -Skip 68
            }else{
                $data = $data | Select-Object -First 17 -Skip 51
            }
        }

        $data | Add-Content -Path $storeCSV
    }
    Remove-Item $year -Recurse
    write-host "Done!`n"
}
