import argparse
from snds_mapping import *
import os

parser = argparse.ArgumentParser()
parser.add_argument("path", help="Path to the xlsx ou csv file containing the top atc drugs")
args = parser.parse_args()
path = args.path
if '.xlsx' in path:
    df = pd.read_excel(path)
elif '.csv' in path:
    df = pd.read_csv(path)
else:
    raise NameError('Path must be to a xlsx or csv file')

df_cleaned = preprocess_df(df)
df_info = create_infotab(df)
df_mapped  = atc_to_pubchemID(df_cleaned)
#Save to excel sheets
if not os.path.isfile('docs/top_atc5_snds_mapped.xlsx'):
    df_mapped.to_excel('docs/top_atc5_snds_mapped.xlsx')

writer = pd.ExcelWriter('docs/top_atc5_snds_mapped.xlsx', engine='xlsxwriter')
df_mapped.to_excel(writer,sheet_name='top_150_snds')
df_info.to_excel(writer,sheet_name='decision_rules')
writer.save()
#df_errors.to_excel('core/42_transla_comedic/top_atc5_snds_notmapped.xlsx')