function S = read_namelist(filename)
%   S = READ_NAMELIST(FILENAME) returns the struct S containg namelists and
%   variables in the file FILENAME organised in hierachical way:
%
%                  |--VAR1
%                  |--VAR2
%     |-- NMLST_A--|...
%     |            |--VARNa
%     |
%     |            |--VAR1
%     |-- NMLST_B--|--VAR2
%     |            |...
% S --|     ...    |--VARNb
%     |
%     |            |--VAR1
%     |-- NMLST_M--|--VAR2
%                  |...
%                  |--VARNm
% 
%   Note:  The function can read multidimensioal variables as well. The  
%   function assumes that there is no more than one namelist section per 
%   line. At this time there is no syntax checking functionality so the 
%   function will crash in case of errors.
%    
%   Example:
%       NMLST = read_namelist('OPTIONS.nam');
%       NMLST.NAM_FRAC.XUNIF_NATURE = 0.1;
%       write_namelist(NMlST, 'MOD_OPTIONS.nam');
%
%   Written by:     Darien Pardinas Diaz (darien.pardinas-diaz@monash.edu)
%   Version:        1.0
%   Date:           16 Dec 2011
S = struct();
% Open and read the text file containing the namelists
fid = fopen(filename,'r');
c = 0;
lines = cell(1);
% Read all the text lines in namelist file
while ~feof(fid)
    line = fgetl(fid);
    % Remove comments if any on the line
    idx = find(line == '!');
    if ~isempty(idx),
        line = line(1:idx(1)-1);
    end
    if ~isempty(line),
        c = c + 1;
        lines{c} = line;
    end
end
fclose(fid);
i = 0;
while i < c;    
    % Find a record
    i = i + 1; 
    line = lines{i};
    idx = find(line == '&');
    if ~isempty(idx), % i.e. a namelist start
        line = line(idx(1) + 1:end);
        % find next space
        idx = find(line == ' ');
        if ~isempty(idx),
            namelst = line(1:idx(1) - 1);
            line = line(idx(1) + 1:end);
        else
            namelst = line;
            line = [];
        end
        nmlst_bdy = [];
        idx = strfind(line,'/');
        % Get the variable specification section
        while isempty(idx) && i < c,
            nmlst_bdy = [nmlst_bdy ' ' line]; 
            i = i + 1;
            line = lines{i};
            idx = strfind(line,'/');
        end
        if ~isempty(idx) && idx(1) > 1,
            nmlst_bdy = [nmlst_bdy ' ' line];
        end
        % Parse current namelist (set of variables)
        S.(namelst) = parse_namelist(nmlst_bdy);        
    end
end
function S = parse_namelist(strng)
% Internal function to parse the body text of a namelist section.
% Limitations: the following patterns are prohibited inside the literal
% strings: '.t.' '.f.' '.true.' '.false.' '(:)'
% Get all .true., .t. and .false., .f. to T and F
strng = regexprep(strng,'\.true\.' ,'T','ignorecase'); 
strng = regexprep(strng,'\.false\.','F','ignorecase');
strng = regexprep(strng,'\.t\.','T','ignorecase'); 
strng = regexprep(strng,'\.f\.','F','ignorecase');
% Make evaluable the (:) expression in MATLAB if any
strng = regexprep(strng, '\(:\)', '(1,:)');
[strng, islit] = parse_literal_strings([strng ' ']);
% Find the position of all the '='
eq_idx = find(strng == '=');
nvars = length(eq_idx);
arg_start = eq_idx + 1;
arg_end   = zeros(size(eq_idx));
vars = cell(nvars,1);
S = struct;
% Loop through every variable
for k = 1:nvars,
    i = eq_idx(k) - 1;
    % Move to the left and discard blank spaces
    while strng(i) == ' ', i = i - 1; end
    % Now we are over the variable name or closing parentesis
    j = i;
    if strng(i) == ')',
        while strng(i) ~= '(', i = i - 1; end
        i = i - 1;
        % Move to the left and discard any possible blank spaces
        while strng(i) == ' ', i = i - 1; end
    end
    
    % Now we are over the last character of the variable name
    while strng(i) ~= ' ', i = i - 1; end    
    
    if k > 1, arg_end(k - 1) = i; end    
    vars{k} = ['S.' strng(i + 1: j)];
end
arg_end(end) = length(strng);
% This variables are used in the eval function to evaluate True/False, 
% so don't remove it!
T = '.true.';
F = '.false.';
% Loop through every variable guess varible type
for k = 1:nvars,    
    arg = strng(arg_start(k):arg_end(k));
    arglit = islit(arg_start(k):arg_end(k))';
    
    % Remove commas in non literal string...
    commas = ~arglit & arg == ',';
    if any(commas)
        arg(commas) = ' ';
    end
    
    if any(arglit),
        % We are parsing a variable that is literal string
        arg = ['{' arg '};'];
    elseif ~isempty(find( arg == 'T' | arg == 'F', 1)),
        % We are parsing a boolean variable
        arg = ['{' arg '};'];
    else
        % We are parsing a numerical array
        arg = ['[' arg '];'];
    end
    % Eval the modified syntax in Matlab
    eval([vars{k} ' = ' arg]);
end
function [strng, is_lit] = parse_literal_strings(strng)
% Parse the literal declarations of strings and change to Matlab syntax
len = length(strng);
add_squote = []; % Positions to add a scape single quote on syntax
rem_dquote = []; % Positions to remove a double quote scape on syntax
i = 1;
while i < len,
    if strng(i) == '''',  % Opening string with single quote...
        i = i + 1;
        while i < len && strng(i) ~= '''' || strcmp(strng(i:i+1),'''''') , 
            i = i + 1; 
            if strcmp(strng(i-1:i),''''''),
                i = i + 1;
            end
        end   
    end
    if strng(i) == '"',  % Opening string with double quote...
        strng(i) = ''''; % Change to single quote
        i = i + 1;
        while strng(i) ~= '"' || strcmp(strng(i:i+1),'""') && i < len,
            % Check for a possible sigle quote here
            if strng(i) == '''',
                add_squote = [add_squote i];
            end            
            i = i + 1; 
            if strcmp(strng(i-1:i),'""'),
                rem_dquote = [rem_dquote i-1];
                i = i + 1;
            end
        end
        strng(i) = ''''; % Change to single quote
    end    
    i = i + 1;
end
for i = 1:length(add_squote);
    strng = [strng(1:add_squote(i)) strng(add_squote(i):end)];
end
for i = 1:length(rem_dquote);
    strng = [strng(1:rem_dquote(i)-1) strng(rem_squote(i)+1:end)];
end
% Now everything should be in Matlab string syntax
% Classify syntax as literal or regular expression
i = 1;
len = length(strng);
is_lit = zeros(len,1);
while i < len,
    if strng(i) == '''',  % Opening string with single quote...
        is_lit(i) = 1;
        i = i + 1;
        while i < len && strng(i) ~= '''' || strcmp(strng(i:i+1),''''''), 
            is_lit(i) = 1;
            i = i + 1; 
            if strcmp(strng(i-1:i),''''''),
                is_lit(i) = 1;
                i = i + 1;
            end
        end
        is_lit(i) = 1;    
    end
    i = i + 1;
end