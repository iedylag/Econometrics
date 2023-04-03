import xml.etree.ElementTree as ET
import csv

# Parse the XML file
tree = ET.parse('Team_CHN.xml')
root = tree.getroot()

# Extract data and write to CSV
with open('output.csv', 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    # Write header row
    writer.writerow(['year', 'team_size', 'problem1', 'problem2', 'problem3', 'problem4', 'problem5', 'problem6', 'total', 'rank', 'gold_medals', 'silver_medals', 'bronze_medals', 'honourable_mentions'])
    # Write data rows
    for team in root.findall('./team'):
        year = team.get('year')
        team_size = team.find('team_size').text
        problem1 = team.find('problem1').text
        problem2 = team.find('problem2').text
        problem3 = team.find('problem3').text
        problem4 = team.find('problem4').text
        problem5 = team.find('problem5').text
        problem6 = team.find('problem6').text
        total = team.find('total').text
        rank = team.find('rank').text
        gold_medals = team.find('gold_medals').text
        silver_medals = team.find('silver_medals').text
        bronze_medals = team.find('bronze_medals').text
        honourable_mentions = team.find('honourable_mentions').text

        writer.writerow([year, team_size, problem1, problem2, problem3, problem4, problem5, problem6, total, rank, gold_medals, silver_medals, bronze_medals, honourable_mentions])