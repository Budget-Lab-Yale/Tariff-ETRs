"""Generate CBO tariff specification for cross-validation.

This script runs the CBO tariff spec generation from their conventional-tariff-analysis-model
and saves the output to a CSV file for comparison with Budget Lab rates.
"""

import sys
import os

# Add the CBO repo to Python path
cbo_repo = 'C:/Users/jar335/Documents/Repositories/conventional-tariff-analysis-model'
sys.path.insert(0, cbo_repo)

import pandas as pd
from code.params import get_params
from code.tariffs import generate_tariff_spec

# Load CBO parameters
params = get_params(config_name='default.yaml')

# Load base import data
base_data_path = os.path.join(
    cbo_repo, 'inputs', 'base_imports',
    f'all_countries_{params["year"]}_CY_{params["pull_date"]}.csv'
)
print(f'Loading base data from: {base_data_path}')
base_data = pd.read_csv(base_data_path, dtype={'CTY_CODE': str, 'I_COMMODITY': str})
print(f'Base data shape: {base_data.shape}')

# Generate tariff specification
print('Generating tariff specification...')
tariff_spec = generate_tariff_spec(
    cd=cbo_repo,
    base_data=base_data,
    reciprocals=params['reciprocals'],
    lists=params['lists'],
    rates=params['rates'],
    misc_params=params['misc_params'],
    scenario_name='crossval',
    preset_tariff_spec=None
)

print(f'Tariff spec shape: {tariff_spec.shape}')
print(f'Columns: {tariff_spec.columns.tolist()}')
print(f'Non-zero rates: {(tariff_spec["new_rate"] > 0).sum()}')
print(f'Unique countries: {tariff_spec["CTY_CODE"].nunique()}')
print(f'Unique HTS codes: {tariff_spec["I_COMMODITY"].nunique()}')

# Save to output location
output_path = 'C:/Users/jar335/Documents/Repositories/Tariff-ETRs/scripts/cbo_tariff_spec.csv'
tariff_spec.to_csv(output_path, index=False)
print(f'Saved CBO tariff spec to: {output_path}')

# Summary stats
print('\n--- Summary by country (top 20 by avg rate) ---')
summary = tariff_spec.groupby('CTY_CODE').agg(
    avg_rate=('new_rate', 'mean'),
    max_rate=('new_rate', 'max'),
    n_products=('I_COMMODITY', 'nunique'),
    n_nonzero=('new_rate', lambda x: (x > 0).sum())
).sort_values('avg_rate', ascending=False)
print(summary.head(20).to_string())
