%% Matlab function for reading post-processing files produced
%% by caffa3d.MBRi at different region levels and multi-grid levels

function GR = caffa3dMBRi_gz_15_0002(Folder, FileNameRoot, Regions, MgLevels, nFiles, GridFlag, FieldList, LreadStat)

    StandardFileNameLength = [33, 35, 37]; % test24.rgc.001.mgc.001.flc.020.gz

    if isempty(FieldList)
        FieldList = {'X', 'Xc', 'Vol', 'X', 'Fm', 'U', 'P', 'T', 'Ximb', 'Dimb', 'Vis', 'Tr1', 'Tr2', 'Tr3', 'Tr4', 'Vof'};
    end

    Listing = dir(Folder);

    nItems = length(Listing);

    nFilesListed = 0;
    FilesListed = [];

    for jItem = 1:nItems
        ItemName = Listing(jItem).name;
        nItemName = length(ItemName);

        if any(nItemName == StandardFileNameLength)

            if strcmp(FileNameRoot, ItemName(1:6))
                nFilesListed = nFilesListed + 1;
                FilesListed(nFilesListed).name = ItemName;
            end

        end

    end

    FL = [];
    GR = [];

    if nFilesListed == 0
        disp('No matching files found');
        return
    end

    for jListedFile = 1:nFilesListed
        jListedFileName = FilesListed(jListedFile).name;
        jFileNameRoot = jListedFileName(1:6);
        jFileRegionCounter = str2num(jListedFileName(12:14));
        jFileMgLevelCounter = str2num(jListedFileName(20:22));
        jFileOutputCounter = str2num(jListedFileName(28:end - 3));
        FileWasRequested = 1;
        %
        if not(isempty(Regions))

            if not(any(Regions == jFileRegionCounter))
                FileWasRequested = 0;
            end

        end

        if not(isempty(MgLevels))

            if not(any(MgLevels == jFileMgLevelCounter))
                FileWasRequested = 0;
            end

        end

        ThisIsGridFile = 0;

        if not(isempty(GridFlag))

            if GridFlag

                if strcmp('grd', jListedFileName(24:26))
                    ThisIsGridFile = 1;
                end

            end

        end

        if not(isempty(nFiles)) % % % File numbers were specified ?

            if not(any(nFiles == jFileOutputCounter)) % % % 'this' file number was requested

                if jFileOutputCounter > 0 % % % no, is file number > 0
                    FileWasRequested = 0; % % % yes, discard this file
                else %%% no, file number == 0

                    if not(ThisIsGridFile)
                        FileWasRequested = 0;
                    end

                end

            end

        end

        %
        if FileWasRequested
            [FL, GR] = LoadThisFile(FL, GR, Folder, jListedFileName, jFileRegionCounter, jFileMgLevelCounter, jFileOutputCounter, ThisIsGridFile, FieldList, LreadStat);
        end

    end

    %%% Function for reading each file  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    function [FL, GR] = LoadThisFile(FL, GR, Folder, FileName, Region, MgLevel, OutputNumber, ThisIsGridFile, FieldList, LreadStat)

        %%% Open file. Windows
        ComputerString = computer;

        if any(strcmp(ComputerString, {'PCWIN', 'PCWIN64'}))
            sendm(['gunzip file : ', FileName]);
            gunzip([Folder, '\', FileName]);
            a = fopen([Folder, '\', FileName(1:end - 3)]);
        else
            %%% Open file. Linux
            %sendm(['gunzip file : ',FileName]);
            system(['gzip -dk ', Folder, FileName]);
            % gunzip is slower than gzip -dk
            %gunzip([Folder,FileName]);
            a = fopen([Folder, FileName(1:end - 3)]);
        end

        %sendm(['Opened file : ',FileName]);

        %%% Read Real Type parameters to detect single/double precision
        dum1 = fread(a, 1, 'int32');
        IrlKind = fread(a, 1, 'int32');
        %
        %%% ...check size of record headder :
        if IrlKind == 0
            dumword = 'int64';
            IrlKind = fread(a, 1, 'int32');
        else
            dumword = 'int32';
        end

        %%% ...continue
        %
        IrlKind2 = fread(a, 1, 'int32');
        dum1 = fread(a, 1, dumword);

        if IrlKind == IrlKind2
            %if DoublePrecisionVar==1,
            VarWord = 'double';
            %sendm(['Double precision selected']);
        else
            VarWord = 'single';
            %sendm(['Single precision selected']);
        end

        %%% Read dimensions and parameters
        dum1 = fread(a, 1, dumword);
        Itim = fread(a, 1, 'int32'); %
        Time = fread(a, 1, VarWord); % time
        dt = fread(a, 1, VarWord); % dt
        NblksRG = fread(a, 1, 'int32'); % number of blocks
        MaxIJK = fread(a, 1, 'int32'); % total number of points
        NFI = fread(a, 1, 'int32'); % number of fields
        dum1 = fread(a, 1, dumword);
        %
        dum1 = fread(a, 1, dumword);
        NiBK = fread(a, NblksRG, 'int32'); % NI    1..NBLKS
        NjBK = fread(a, NblksRG, 'int32'); % NJ    1..NBLKS
        NkBK = fread(a, NblksRG, 'int32'); % NK    1..NBLKS
        dum1 = fread(a, 1, dumword);
        %
        dum1 = fread(a, 1, dumword);
        iBK = fread(a, NblksRG, 'int32'); % iBK   1..NBLKS
        jBK = fread(a, NblksRG, 'int32'); % jBK   1..NBLKS
        kBK = fread(a, NblksRG, 'int32'); % kBK   1..NBLKS
        dum1 = fread(a, 1, dumword);
        %
        dum1 = fread(a, 1, dumword);
        NijkBRG = fread(a, NblksRG, 'int32'); % NijkBRG 1..NBLKS
        ijkBRG = fread(a, NblksRG, 'int32'); %  ijkBRG 1..NBLKS
        dum1 = fread(a, 1, dumword);
        %
        dum1 = fread(a, 1, dumword);
        kMgLevel = fread(a, 1, 'int32'); %  kMgLevel
        NmgLevels = fread(a, 1, 'int32'); %  NmgLevels
        NblksMG = fread(a, 1, 'int32'); %  NblksMG
        iBlksMGRG = fread(a, NmgLevels, 'int32'); %  iBlksMGRG
        iStrMGRG = fread(a, NmgLevels, 'int32'); %  iStrMGRG
        iEndMGRG = fread(a, NmgLevels, 'int32'); %  iEndMGRG
        dum1 = fread(a, 1, dumword);
        %
        dum1 = fread(a, 1, dumword);
        LwGrid = fread(a, 1, 'int32'); % LwGrid   1
        LwGridXr = fread(a, 1, 'int32'); % LwGridXr 1
        LwGridBc = fread(a, 1, 'int32'); % LwGridBc 1
        LwFm = fread(a, 1, 'int32'); % LwFm     1
        LwGrad = fread(a, 1, 'int32'); % LwGrad   1
        LwResmon = fread(a, 1, 'int32'); % LwResmon 1
        dum1 = fread(a, 1, dumword);
        %
        dum1 = fread(a, 1, dumword);
        LwCal = fread(a, NFI, 'int32'); % LwCAL 1..NFI
        %LRADIATION=fread(a,1,'int32');  % LRADIATION 1
        dum1 = fread(a, 1, dumword);
        %

        %%% Read Basic Grid Data and sort in blocks

        if LwGrid
            dum1 = fread(a, 1, dumword);
            GR = ReadFieldFromFile(a, 'single', GR, 'X', 3, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            GR = ReadFieldFromFile(a, 'single', GR, 'Xc', 3, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            GR = ReadFieldFromFile(a, 'single', GR, 'Vol', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LwGrid && LwGridXr
            dum1 = fread(a, 1, dumword);
            %...
            %...
            dum1 = fread(a, 1, dumword);
        end

        %sendm(['Done with grid data : ',FileName]);

        if ThisIsGridFile

            fclose(a);

            %%% Delete unpacked file. Windows
            ComputerString = computer;

            if any(strcmp(ComputerString, {'PCWIN', 'PCWIN64'}))
                %sendm(['gunzip file : ',FileName]);
                %gunzip([Folder,'\',FileName]);
                %a=fopen(filnam);
                %a=fopen([Folder,'\',FileName(1:end-3)]);
                delete([Folder, '\', FileName(1:end - 3)])
            else
                %%% Compress file again
                %         eval(['!cp ',Folder,'/',FileName,' ../matlab/']);
                %         eval(['!gunzip ./',FileName]);
                %         a=fopen(FileName(1:end-3));
                delete([Folder, FileName(1:end - 3)])
            end

            return;
        end

        %%% Read Flow data and sort in blocks

        Ivof = 10;
        Ibgh = 15;
        Icgm = 24;

        if LwFm
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'Fm', 3, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LwCal(1)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'U', 3, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LwCal(4)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'P', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LwCal(5)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'T', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LwCal(6) || LwCal(Ivof) || LwCal(Ibgh)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'Vis', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        % if LwCal(6)|LwCal(Ivof)|LwCal(Ibgh)|LwCal(Icgm),
        %     dum1=fread(a,1,dumword);
        %     FL=ReadFieldFromFile(a,VarWord,FL,'Gen' ,1,Region,MgLevel,iBlksMGRG,NblksMG,NijkBRG,ijkBRG,NkBK,NiBK,NjBK,iStrMGRG,iEndMGRG,FieldList);
        %     dum1=fread(a,1,dumword);
        % end;

        Imbc = 17;

        if LwCal(Imbc)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'Ximb', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'Dimb', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LwCal(Ivof)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'Vof', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        Iqv = 11; Iqc = 12;

        if LwCal(Iqv)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'Qv', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LwCal(Iqc)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'Qc', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LwCal(Iqc)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'RadSrco', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        Itr4 = 13;
        Itr1 = 18;
        Itr2 = 19;
        Itr3 = 20;

        if LwCal(Itr1)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'Tr1', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LwCal(Itr2)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'Tr2', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LwCal(Itr3)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'Tr3', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LwCal(Itr4)
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'Tr4', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LwResmon
            dum1 = fread(a, 1, dumword);
            FL = ReadFieldFromFile(a, VarWord, FL, 'Resmon', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
            dum1 = fread(a, 1, dumword);
        end

        if LreadStat

            if LwCal(1)
                dum1 = fread(a, 1, dumword);
                FL = ReadFieldFromFile(a, VarWord, FL, 'UMean', 3, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
                dum1 = fread(a, 1, dumword);
                dum1 = fread(a, 1, dumword);
                FL = ReadFieldFromFile(a, VarWord, FL, 'UUMean', 6, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
                dum1 = fread(a, 1, dumword);
            end

            if LwCal(1)
                dum1 = fread(a, 1, dumword);
                FL = ReadFieldFromFile(a, VarWord, FL, 'VisMean', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
                dum1 = fread(a, 1, dumword);
            end

            if LwCal(Itr1)
                dum1 = fread(a, 1, dumword);
                FL = ReadFieldFromFile(a, VarWord, FL, 'Tr1Mean', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
                dum1 = fread(a, 1, dumword);
            end

            if LwCal(Itr2)
                dum1 = fread(a, 1, dumword);
                FL = ReadFieldFromFile(a, VarWord, FL, 'Tr2Mean', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
                dum1 = fread(a, 1, dumword);
            end

            if LwCal(Itr3)
                dum1 = fread(a, 1, dumword);
                FL = ReadFieldFromFile(a, VarWord, FL, 'Tr3Mean', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
                dum1 = fread(a, 1, dumword);
            end

            if LwCal(Itr4)
                dum1 = fread(a, 1, dumword);
                FL = ReadFieldFromFile(a, VarWord, FL, 'Tr4Mean', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
                dum1 = fread(a, 1, dumword);
            end

            if LwCal(5)
                dum1 = fread(a, 1, dumword);
                FL = ReadFieldFromFile(a, VarWord, FL, 'TMean', 1, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);
                dum1 = fread(a, 1, dumword);
            end

        end

        FL(1).Time = Time;

        fclose all;

        %%% Delete unpacked file. Windows
        ComputerString = computer;

        if any(strcmp(ComputerString, {'PCWIN', 'PCWIN64'}))
            %sendm(['gunzip file : ',FileName]);
            %gunzip([Folder,'\',FileName]);
            %a=fopen(filnam);
            %a=fopen([Folder,'\',FileName(1:end-3)]);
            delete([Folder, '\', FileName(1:end - 3)])
        else
            %%% Open file. Linux
            %eval(['!cp ',Folder,'/',FileName,' ../matlab/']);
            %eval(['!gunzip ./',FileName]);
            %a=fopen(FileName(1:end-3));
            %delete(FileName);
            delete([Folder, FileName(1:end - 3)])
        end

        return

        %%% Function for reading each field from file

        function Data = ReadFieldFromFile(FileHandle, TypeWord, Data, FieldName, FieldDim, Region, MgLevel, iBlksMGRG, NblksMG, NijkBRG, ijkBRG, NkBK, NiBK, NjBK, iStrMGRG, iEndMGRG, FieldList);

            Offset = -iStrMGRG(MgLevel) + 1;
            FieldCount = iEndMGRG(MgLevel) - iStrMGRG(MgLevel) + 1;

            Field = fread(FileHandle, FieldDim * FieldCount, TypeWord);
            Field = reshape(Field, [FieldDim, FieldCount]);

            if (any(strcmp(FieldName, FieldList)))
                BlockCounter = 0;

                for jBlock = iBlksMGRG(MgLevel) + 1:iBlksMGRG(MgLevel) + NblksMG
                    BlockCounter = BlockCounter + 1;
                    Data(BlockCounter, MgLevel, Region).(FieldName) = permute(reshape(Field(:, Offset + ijkBRG(jBlock) + 1:Offset + ijkBRG(jBlock) + NijkBRG(jBlock)), [FieldDim, NjBK(jBlock), NiBK(jBlock), NkBK(jBlock)]), [2, 3, 4, 1]);
                    Data(BlockCounter, MgLevel, Region).BlockIndex = jBlock;
                end

            end

        end

    end

    GR = rmfield(GR, {'X', 'BlockIndex', 'Vol'});
    GR = GR(Regions);

end
