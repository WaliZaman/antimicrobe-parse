set -e
echo 'python parse.py'
python parse.py
echo 'python parse_square.py'
python parse_square.py
echo 'python parse_adhumic.py'
python parse_adhumic.py
echo 'python parse_birdem.py'
python parse_birdem.py
echo 'python parse.py'
python parse.py
echo 'python clean.py'
python clean.py
echo 'R < amr_preprocess.r --no-save'
R < amr_preprocess.r --no-save
echo 'python prepare_matrix.py'
python prepare_matrix.py
echo 'Done!'
