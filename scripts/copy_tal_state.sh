#!/usr/bin/env bash
scp -r crowdsa@tal:PaperValidator/public/papers/* public/papers
scp -r crowdsa@tal:PaperValidator/tmp .
ssh tal mysqldump -u papval_readonly -ppapval_readonly papval | mysql -u root papervalidator
thepath=`pwd`
mysql -u root papervalidator <<EOF
UPDATE permutations SET snippet_filename = REPLACE(snippet_filename, '/home/crowdsa/PaperValidator', '$thepath'), pdf_path = REPLACE(pdf_path, '/home/crowdsa', '$thepath');
EOF