% dataset(DirectoryName)
% this is where the image dataset is located
% locally defined for your convenience. no path switching required
dataset("./imageDataset2_15_20/").

% prepares file so you can use it without needing to type the long file path
% syntax: prepare_file("q00.jpg.txt", Preparedfile)
prepare_file(Filename, PFile):-
    string_concat("./queryImagesWithHistograms/queryImages/", Filename, PFile).

% directory_textfiles(DirectoryName, ListOfTextfiles)
% produces the list of text files in a directory
directory_textfiles(D,Textfiles):- directory_files(D,Files), include(isTextFile, Files, Textfiles).
isTextFile(Filename):- string_concat(_,'.txt',Filename).
% read_hist_file(Filename,ListOfNumbers)
% reads a histogram file and produces a list of numbers (bin values)
read_hist_file(Filename,Numbers):- open(Filename,read,Stream),read_line_to_string(Stream,_),
                                   read_line_to_string(Stream,String), close(Stream),
								   atomic_list_concat(List, ' ', String),atoms_numbers(List,Numbers).
								   
% similarity_search(QueryFile,SimilarImageList)
% returns the list of images similar to the query image
% similar images are specified as (ImageName, SimilarityScore)
% predicat dataset/1 provides the location of the image set
similarity_search(QueryFile,SimilarList) :- dataset(D), directory_textfiles(D,TxtFiles),
                                            similarity_search(QueryFile,D,TxtFiles,SimilarList).
											
% similarity_search(QueryFile, DatasetDirectory, HistoFileList, SimilarImageList)
similarity_search(QueryFile,DatasetDirectory, DatasetFiles,Best):- read_hist_file(QueryFile,QueryHisto), 
                                            compare_histograms(QueryHisto, DatasetDirectory, DatasetFiles, Scores), 
                                            sort(2,@>,Scores,Sorted),take(Sorted,5,Best).

% compare_histograms(QueryHisto,DatasetDirectory,DatasetFiles,Scores)
% compares a query histogram with a list of histogram files 
compare_histograms(QueryHisto, DatasetDirectory, DatasetFiles, Scores):-
    
    % Call supplementary comparer that will do most of the work
    compare_histograms(QueryHisto, DatasetDirectory, DatasetFiles, [], Scores).

% supplementary comparer. recursively compares every file to the queryhisto, then returns the scores
% in doing so, it calls the relevant sub-predicates
compare_histograms(_, _, [], Scores, Scores).
compare_histograms(QueryHisto, DatasetDirectory, [Headfile|Tailfiles], TempScores, Scores):-    

    % gets the proper directory for the current head file
    string_concat(DatasetDirectory, Headfile, Currentfile),
    % reads in the histogram for the current file
    read_hist_file(Currentfile, Currenthistfile),
    % gets the intersecting score
    histogram_intersection(QueryHisto, Currenthistfile, Currentscore),
    % recursively continues the rest of the datasets
    compare_histograms(QueryHisto, DatasetDirectory, Tailfiles, [(Headfile, Currentscore)|TempScores], Scores).

% histogram_intersection(Histogram1, Histogram2, Score)
% compute the intersection similarity score between two histograms
% Score is between 0.0 and 1.0 (1.0 for identical histograms)
histogram_intersection(H1,H2,S):-
    sum_list(H1, Pixels1), sum_list(H2, Pixels2),
    calculate_score(H1, H2, Pixels1, Pixels2, 0, S).

% histogram_intersection mainly just gathers the sum of the pixels for calculation
% this predicate calculates the score of the query histogram 1 and another histogram 2.
% end case return the Tempscore as final score S
calculate_score([], [], _, _, S, S).
% normalize heads, add the smallest number to the Tempscore
calculate_score([Head1|Tail1], [Head2|Tail2], Pixels1, Pixels2, TempScore, S):-
    NH1 is Head1 / Pixels1, NH2 is Head2 / Pixels2,
    NewTemp is TempScore + min(NH1, NH2),
    calculate_score(Tail1, Tail2, Pixels1, Pixels2, NewTemp, S).

% take(List,K,KList)
% extracts the K first items in a list
take(Src,N,L) :- findall(E, (nth1(I,Src,E), I =< N), L).
% atoms_numbers(ListOfAtoms,ListOfNumbers)
% converts a list of atoms into a list of numbers
atoms_numbers([],[]).
atoms_numbers([X|L],[Y|T]):- atom_number(X,Y), atoms_numbers(L,T).
atoms_numbers([X|L],T):- \+atom_number(X,_), atoms_numbers(L,T).
