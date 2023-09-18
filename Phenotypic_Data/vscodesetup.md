# Setting up R studio in VScode
1. Install the R extention in vscode

2. Install a special package
    ```
    install.packages("languageserver")
    ```

4. Set the R path in Vscode to access r terminal
    #C:\Program Files\RStudio\rstudio.exe  
    #C:\Users\xxxxx\AppData\Local\Programs\R\R-4.2.1\bin\R.exe
    #C:\Program Files\R\R-4.3.1\bin\x64\Rgui.exe
    #C:\Program Files\R\R-4.3.1\bin\x64\Rterm.exe

5. To allow plotting in vscode you need to install a package httpgd in r

6. Go to r > plot in vscode settings and mark "use httpgd"