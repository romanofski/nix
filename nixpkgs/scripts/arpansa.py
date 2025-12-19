#!/usr/bin/env python
import urllib.request
import argparse
import json
import xml.etree.ElementTree as ET

UV_XML_URL = 'https://uvdata.arpansa.gov.au/xml/uvvalues.xml'

def main(location: str):
    with urllib.request.urlopen(UV_XML_URL) as f:
        tree = ET.parse(f)
        element = tree.find(f'./location[@id="{location}"]/index')
        uv = '--'
        if element is None or not element.text:
            uv = 'NO-EL'
        else:
           uv = element.text

        print(json.dumps({
            "text": uv,
            "tooltip": uv,
            "class": "arpansa-uv"

        }))

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("location", type=str)
    parsed = parser.parse_args()
    main(**parsed.__dict__)
