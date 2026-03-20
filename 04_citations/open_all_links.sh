#!/bin/bash
# Open all citation links in Safari for download via Stanford SSO
# Run: bash open_all_links.sh

echo "Opening SSRN papers..."
open "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5933475"  # Gomez Cram 2025
open "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2322420"  # Rothschild & Sethi 2016
open "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3111607"  # Martineau 2022
open "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=508943"   # Richardson 2004
open "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=298868"   # Matsumoto 2002
open "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=521447"   # Ke & Yu 2006
open "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2333671"  # Jame 2016
open "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4538738"  # Schafhautle & Veenman 2024
open "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=345420"   # Heflin 2003

sleep 2
echo "Opening JSTOR papers (use Stanford proxy if needed)..."
open "https://www-jstor-org.stanford.idm.oclc.org/stable/2117471"   # Forsythe 1992
open "https://www-jstor-org.stanford.idm.oclc.org/stable/2490232"   # Ball & Brown 1968
open "https://www-jstor-org.stanford.idm.oclc.org/stable/2491062"   # Bernard & Thomas 1989
open "https://www-jstor-org.stanford.idm.oclc.org/stable/1913210"   # Kyle 1985
open "https://www-jstor-org.stanford.idm.oclc.org/stable/1809376"   # Hayek 1945
open "https://www-jstor-org.stanford.idm.oclc.org/stable/2491338"   # Skinner 1994

sleep 2
echo "Opening publisher DOIs (use Stanford proxy if needed)..."
open "https://doi-org.stanford.idm.oclc.org/10.1111/j.1540-6261.1991.tb02683.x"  # Lee & Ready 1991
open "https://doi-org.stanford.idm.oclc.org/10.1016/0304-405X(87)90029-8"         # Easley & O'Hara 1987
open "https://doi-org.stanford.idm.oclc.org/10.1093/rfs/hhs053"                    # Easley et al 2012
open "https://doi-org.stanford.idm.oclc.org/10.1111/j.1475-679X.2006.00196.x"     # Livnat & Mendenhall 2006
open "https://doi-org.stanford.idm.oclc.org/10.1111/j.1540-6261.1997.tb03807.x"   # Shleifer & Vishny 1997
open "https://doi-org.stanford.idm.oclc.org/10.1016/0304-405X(93)90023-5"          # Fama & French 1993
open "https://doi-org.stanford.idm.oclc.org/10.1016/j.finmar.2013.06.006"          # Menkveld 2013

# Brier 1950 is open access
open "https://journals.ametsoc.org/view/journals/mwre/78/1/1520-0493_1950_078_0001_vofeit_2_0_co_2.xml"

echo ""
echo "Done! 24 tabs opened. Download PDFs to:"
echo "  $(pwd)/pdfs/"
echo ""
echo "Suggested filenames are in download_links.md"
