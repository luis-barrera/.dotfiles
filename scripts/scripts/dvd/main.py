import pygame as pg
import sys
import random as rand

pg.init()

size = width, height = 600, 600
screen = pg.display.set_mode(size)

speed = [5, 4]
clock = pg.time.Clock()

colors = ['blue', 'green', 'orange', 'pink', 'red']
logo = pg.image.load('resized/' + colors[0] + '.png')
rect = logo.get_rect()

fps = 30
count = 0
change_color = 3 * fps 

while True:
    for event in pg.event.get():
        if event.type == pg.QUIT:
            sys.exit()

    new_size = pg.display.Info().current_w, pg.display.Info().current_h
    if new_size[0] != size[0] or new_size[1] != size[1]:

        size = width, height = new_size[0], new_size[1]
        screen = pg.display.set_mode(size)

    if rect.left < 0:
        speed[0] = -speed[0]
    if rect.right > width:
        speed[0] = -speed[0]
    if rect.top < 0:
        speed[1] = -speed[1]
    if rect.bottom > height:
        speed[1] = -speed[1]

    if count == change_color:
        logo = pg.image.load('resized/' + colors[rand.randint(0,4)] + '.png')
        count = 0
    else: 
        count += 1

    rect.left += speed[0]
    rect.top += speed[1]

    screen.fill((0,0,0))
    screen.blit(logo, rect)

    pg.display.update()

    clock.tick(fps)

