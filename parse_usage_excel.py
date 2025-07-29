import zipfile
import xml.etree.ElementTree as ET
import re
import json
from typing import Dict

NS = {'m': 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'}


def _read_shared_strings(z: zipfile.ZipFile) -> list:
    root = ET.fromstring(z.read('xl/sharedStrings.xml'))
    return [t.text or '' for t in root.iter('{%s}t' % NS['m'])]


def parse_mapping(xlsx_path: str) -> Dict[str, str]:
    z = zipfile.ZipFile(xlsx_path)
    shared = _read_shared_strings(z)
    sheet = ET.fromstring(z.read('xl/worksheets/sheet1.xml'))
    mapping: Dict[str, str] = {}
    for row in sheet.findall('m:sheetData/m:row', NS):
        cells = {}
        for c in row.findall('m:c', NS):
            col = re.match(r'([A-Z]+)', c.attrib['r']).group(1)
            t = c.attrib.get('t')
            v = c.find('m:v', NS)
            if v is None:
                continue
            val = v.text
            if t == 's':
                val = shared[int(val)]
            cells[col] = val
        rule = cells.get('B')
        auth_obj = cells.get('F')
        auth_field = cells.get('G')
        auth_val = cells.get('H')
        if rule and auth_obj and auth_field and auth_val:
            if rule.startswith(('GD', 'GB', 'GC')):
                level = rule[:2]
                key = f"{auth_obj}|{auth_field}|{auth_val}"
                mapping[key] = level
    return mapping


def main():
    mapping = parse_mapping('PCE_RISE_V1.69.XLSX')
    with open('usage_mapping.json', 'w', encoding='utf-8') as f:
        json.dump(mapping, f, ensure_ascii=False, indent=2)


if __name__ == '__main__':
    main()
