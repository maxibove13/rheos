function [XC, YC, ZC] = checkGridMBo8o3008(GridFileName)

    a = fopen([GridFileName]);

    %%% Read some parameters and dimensions
    dum1 = fread(a, 1, 'int32');
    NXA = fread(a, 1, 'int32');
    %
    %%% ...check size of record headder :
    if NXA == 0,
        dumword = 'int64';
        NXA = fread(a, 1, 'int32');
    else
        dumword = 'int32';
        end;
        %%% ...continue
        %
        NYA = fread(a, 1, 'int32');
        NZA = fread(a, 1, 'int32');
        NXYZA = fread(a, 1, 'int32');
        NIA = fread(a, 1, 'int32');
        NOA = fread(a, 1, 'int32');
        NSA = fread(a, 1, 'int32');
        NWA = fread(a, 1, 'int32');
        NUA = fread(a, 1, 'int32');
        NGA = fread(a, 1, 'int32');
        NOCA = fread(a, 1, 'int32');
        NWALI = fread(a, 1, 'int32');
        NWALA = fread(a, 1, 'int32');
        NWALF = fread(a, 1, 'int32');
        NIAX = fread(a, 1, 'int32');
        NOAX = fread(a, 1, 'int32');
        NSAX = fread(a, 1, 'int32');
        NWAX = fread(a, 1, 'int32');
        NPRX = fread(a, 1, 'int32');
        NTGX = fread(a, 1, 'int32');
        NOCX = fread(a, 1, 'int32');
        dum1 = fread(a, 1, dumword);

        %%% Read some indexes
        dum1 = fread(a, 1, dumword);
        LI = fread(a, NXA, 'int32');
        LK = fread(a, NZA, 'int32');
        dum1 = fread(a, 1, dumword);

        %%% Read Indexes for B.C.s
        dum1 = fread(a, 1, dumword);
        IJI = fread(a, NIAX, 'int32');
        IJPI = fread(a, NIAX, 'int32');
        IJI1 = fread(a, NIAX, 'int32');
        IJI2 = fread(a, NIAX, 'int32');
        IJI3 = fread(a, NIAX, 'int32');
        IJI4 = fread(a, NIAX, 'int32');
        ITAGI = fread(a, NIAX, 'int32');
        dum1 = fread(a, 1, dumword);
        %
        dum1 = fread(a, 1, dumword);
        IJO = fread(a, NOAX, 'int32');
        IJPO = fread(a, NOAX, 'int32');
        IJO1 = fread(a, NOAX, 'int32');
        IJO2 = fread(a, NOAX, 'int32');
        IJO3 = fread(a, NOAX, 'int32');
        IJO4 = fread(a, NOAX, 'int32');
        ITAGO = fread(a, NOAX, 'int32');
        dum1 = fread(a, 1, dumword);
        %
        dum1 = fread(a, 1, dumword);
        IJS = fread(a, NSAX, 'int32');
        IJPS = fread(a, NSAX, 'int32');
        IJS1 = fread(a, NSAX, 'int32');
        IJS2 = fread(a, NSAX, 'int32');
        IJS3 = fread(a, NSAX, 'int32');
        IJS4 = fread(a, NSAX, 'int32');
        ITAGS = fread(a, NSAX, 'int32');
        dum1 = fread(a, 1, dumword);
        %
        dum1 = fread(a, 1, dumword);
        IJW = fread(a, NWAX, 'int32');
        IJPW = fread(a, NWAX, 'int32');
        IJW1 = fread(a, NWAX, 'int32');
        IJW2 = fread(a, NWAX, 'int32');
        IJW3 = fread(a, NWAX, 'int32');
        IJW4 = fread(a, NWAX, 'int32');
        ITAGW = fread(a, NWAX, 'int32');
        dum1 = fread(a, 1, dumword);
        %
        dum1 = fread(a, 1, dumword);
        IJU = fread(a, NPRX, 'int32');
        IJPU = fread(a, NPRX, 'int32');
        IJU1 = fread(a, NPRX, 'int32');
        IJU2 = fread(a, NPRX, 'int32');
        IJU3 = fread(a, NPRX, 'int32');
        IJU4 = fread(a, NPRX, 'int32');
        ITAGU = fread(a, NPRX, 'int32');
        dum1 = fread(a, 1, dumword);
        %
        dum1 = fread(a, 1, dumword);
        IJG = fread(a, NTGX, 'int32');
        IJPG = fread(a, NTGX, 'int32');
        IJG1 = fread(a, NTGX, 'int32');
        IJG2 = fread(a, NTGX, 'int32');
        IJG3 = fread(a, NTGX, 'int32');
        IJG4 = fread(a, NTGX, 'int32');
        ITAGG = fread(a, NTGX, 'int32');
        dum1 = fread(a, 1, dumword);
        %
        dum1 = fread(a, 1, dumword);
        IJL = fread(a, NOCX, 'int32');
        IJR = fread(a, NOCX, 'int32');
        IJOC1 = fread(a, NOCX, 'int32');
        IJOC2 = fread(a, NOCX, 'int32');
        IJOC3 = fread(a, NOCX, 'int32');
        IJOC4 = fread(a, NOCX, 'int32');
        ITAGOC = fread(a, NOCX, 'int32');
        dum1 = fread(a, 1, dumword);
        %
        %%% Read grid data
        dum1 = fread(a, 1, dumword);
        X = fread(a, NXYZA, 'single');
        Y = fread(a, NXYZA, 'single');
        Z = fread(a, NXYZA, 'single');
        XC = fread(a, NXYZA, 'single');
        YC = fread(a, NXYZA, 'single');
        ZC = fread(a, NXYZA, 'single');
        FEE = fread(a, NXYZA, 'single');
        FEN = fread(a, NXYZA, 'single');
        FET = fread(a, NXYZA, 'single');
        FNE = fread(a, NXYZA, 'single');
        FNN = fread(a, NXYZA, 'single');
        FNT = fread(a, NXYZA, 'single');
        FTE = fread(a, NXYZA, 'single');
        FTN = fread(a, NXYZA, 'single');
        FTT = fread(a, NXYZA, 'single');
        VOL = fread(a, NXYZA, 'single');
        SRDW = fread(a, NWAX, 'single');
        XNW = fread(a, NWAX, 'single');
        YNW = fread(a, NWAX, 'single');
        ZNW = fread(a, NWAX, 'single');
        SRDS = fread(a, NSAX, 'single');
        XNS = fread(a, NSAX, 'single');
        YNS = fread(a, NSAX, 'single');
        ZNS = fread(a, NSAX, 'single');
        dum1 = fread(a, 1, dumword);
        %

        fclose(a);

        X = reshape(X, NYA, NXA, NZA);
        Y = reshape(Y, NYA, NXA, NZA);
        Z = reshape(Z, NYA, NXA, NZA);
        XC = reshape(XC, NYA, NXA, NZA);
        YC = reshape(YC, NYA, NXA, NZA);
        ZC = reshape(ZC, NYA, NXA, NZA);
        FEE = reshape(FEE, NYA, NXA, NZA);
        FEN = reshape(FEN, NYA, NXA, NZA);
        FET = reshape(FET, NYA, NXA, NZA);
        FNE = reshape(FNE, NYA, NXA, NZA);
        FNN = reshape(FNN, NYA, NXA, NZA);
        FNT = reshape(FNT, NYA, NXA, NZA);
        FTE = reshape(FTE, NYA, NXA, NZA);
        FTN = reshape(FTN, NYA, NXA, NZA);
        FTT = reshape(FTT, NYA, NXA, NZA);
        VOL = reshape(VOL, NYA, NXA, NZA);
