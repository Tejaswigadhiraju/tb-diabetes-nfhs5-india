# Data

The raw NFHS-5 files are not in this repository. The DHS Program does not allow them to be shared, so you need to request them yourself.

1. Register at the DHS Program website (https://dhsprogram.com) and ask for access to the India 2019-21 (NFHS-5) datasets.
2. Download the Stata (.DTA) versions of
   - the individual (women's) recode, `IAIR7EFL.DTA`
   - the men's recode, `IAMR7EFL.DTA`
   - the household member (PR) recode, `IAPR7EFL.DTA`
3. Put them in the project folder exactly like this. The folder names matter because the scripts use them.

```
Raw_data_women/IAIR7EFL.DTA
Raw_data_men/IAMR7EFL.DTA
Raw_data_household/IAPR7EFL.DTA
```

The letter before "FL" in a DHS file name (here "E") is the release version. If your download has a different letter, rename the files or change the file names in scripts 01 to 05, 08b, 11 and 19, which read them.

## State boundary file (for the maps)

Scripts 12 and 12b need a state-level (ADM1) boundary file saved as

```
Raw_data_household/india_states.geojson
```

I used the geoBoundaries ADM1 file for India from the Humanitarian Data Exchange (https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-india). The state name column in that file is `shapeName`. Check the licence on the download page before reusing it. This file is also left out of the repository.

## No DHS access yet?

`tests/make_mock_data.R` writes fake files with the same names, variables and codes, so you can run the whole pipeline and see that the code works. The numbers it produces are meaningless. See the main README.

## Folders the scripts write to

`Clean_data/`, `Tables/` and `Figures/` already exist in this repository, empty. The scripts write into them. Cleaned person-level files in `Clean_data/` are not uploaded (see `.gitignore`).
