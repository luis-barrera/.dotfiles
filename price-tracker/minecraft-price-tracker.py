import requests
from bs4 import BeautifulSoup

file = open("/home/luisbarrera/linux-dotfiles/price-tracker/minecraft.txt",
            "r+")
lines = file.readlines()
precio_viejo = lines[-1]

url = 'https://www.eneba.com/other-minecraft-official-website-key-global'

headers = {
    "User-Agent":
    'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko)Chrome/83.0.4103.122 Safari/537.36'
}


def get_page():
    page = requests.get(url, headers=headers)
    soup = BeautifulSoup(page.content, 'html5lib')
    return soup


def get_price():
    soup = get_page()
    prices = list(soup.find_all('span', '_3RZkEb'))
    if not prices:
        get_price()
    else:
        price = prices[2].get_text()[1:]
        return float(price)


precio_nuevo = get_price()

if precio_nuevo < float(precio_viejo):
    print("Bajó a €{}".format(precio_nuevo))
    file.write(str(precio_nuevo) + "\n")
else:
    print("€{}".format(precio_nuevo))
    file.write(str(precio_nuevo) + "\n")

file.close()
