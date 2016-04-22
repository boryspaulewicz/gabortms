## -*- coding: utf-8 -*-

## TODO: catch triale = czas prezentacji 0, żeby sprawdzić, czy nie
## podają zbyt wysokiej pewności gdy nic nie ma.

## Kolejność zawsze gabor, skala. Kryjemy gabora maską - szachownicą.
## Cośtam o dostosowaniu czasu prezentacji maski, żeby cośtam było 328

## Dwa treningi: czasy standardowo 4 Trening odpowiedzi: 500 ms x 8
## prób Trening ze skalą: 4 czasy x 16 prób - zapisywanie danych już
## dla treningu ze skalą


if(interactive())source('~/cs/code/r/tasks/task/task.R')

## Globalne parametry zadania

FIXATION.TIME = 1500
POST.STIM.TIME = 0
LONELY.MASK.DURATION = 0
SCALE.MAX.DURATION = 100000
MAX.REACTION.TIME = 4000
FEEDBACK.TIME = 1000
FITTING.START = 20
TARGET.ACC = .8
block.length = 24

## Parametry rysowania gabora i skali

sigma = .01
f = 20
contrast = .3
scale.position = .75

## Globalne obiekty graficzne

TXT$set.string("Proszę nacisnąć spację")
center(TXT, WINDOW)
FX = fixation(WINDOW, size = .02)
STIM = new(Text)
STIM$set.font(FONT)

i = new(Image)
mask = new(Image)
## Tworzymy tylko tyle obrazka, ile potrzeba
for(obj in c(i, mask)){
    obj$create(WINDOW$get.size()[1] * 6 * sigma, WINDOW$get.size()[1] * 6 * sigma, c(0, 0, 0))
}
draw.sin(i, f = f, 45, sigma = sigma, contrast = contrast, mask = F)
draw.sin(mask, f = f, 45, sigma = sigma, mask = T)
i.texture = new(Texture)
i.texture$create(i$size[1], i$size[2])
i.texture$update(i, 0, 0)
mask.texture = new(Texture)
mask.texture$create(mask$size[1], mask$size[2])
mask.texture$update(mask, 0, 0)
s = new(Sprite)
s$set.texture(i.texture, F)
center(s, WINDOW)
m = new(Sprite)
m$set.texture(mask.texture, F)
center(m, WINDOW)

## Funkcje pomocnicze, typu rysowanie bodźców

draw.stim = function(side){
    if(side == 'left'){
        s$set.rotation(-90)
    }else{
        s$set.rotation(0)
    }
    WINDOW$draw(s)
}

## Dwa klawisze w kluczu reakcyjnym

KEYS <<- c(Key.Left, Key.Right)

dane = model = new.contrast = NULL
trial.code = function(trial, side = 'left', contrast = .5, duration = 32, withscale = 1, feedback = 0, scale = 'pas', stage = 'unknown'){
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    if(trial == 1){
        dane <<- data.frame(contrast = NA, acc = rep(NA, 400))
        state = 'press-space'
    }else if((trial %% block.length) == 0){
        state = 'break'
    }else{ state = 'show-fixation' }
    if(trial > 50){
        draw.sin(i, f = f, 45, sigma = sigma, contrast = new.contrast, mask = F)
    }
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        if(KEY.PRESSED[Key.Escape + 1] > start)return(NULL)
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            WINDOW$clear(c(.5, .5, .5))
            TXT$set.string("Naciśnij spację aby rozpocząć zadanie")
            WINDOW$draw(center.win(TXT))
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'break' = {
            WINDOW$clear(c(.5, .5, .5))
            TXT$set.string("Krótka przerwa - odpocznij. Aby kontynuować, naciśnij spację")
            WINDOW$draw(center.win(TXT))
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(.5, .5, .5))
            ## Losowa pozycja myszki
            mouse.set.position(c(runif(1), .5) * WINDOW$get.size())
            ## Punkt fiksacji
            lapply(FX, WINDOW$draw)
            WINDOW$set.mouse.cursor.visible(F)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$set.mouse.cursor.visible(F)
                WINDOW$clear(c(.5, .5, .5))
                WINDOW$display()
                state = 'show-gabor'
                fixation.cleared = CLOCK$time
            }
        }, 'show-gabor' = {
            WINDOW$clear(c(.5, .5, .5))
            draw.stim(side)
            WINDOW$display()
            stim.onset = CLOCK$time
            state = 'gabor-present'
        }, 'gabor-present' = {
            if((CLOCK$time - stim.onset) > duration){
                ## Znikamy gabora
                WINDOW$clear(c(.5, .5, .5))
                WINDOW$display()
                stim.cleared = CLOCK$time
                state = 'post-gabor'
            }
        }, 'post-gabor' = { ## tu rysujemy maskę
            if((CLOCK$time - stim.cleared) > POST.STIM.TIME){
                ## WINDOW$draw(m) ##! Bez maski
                ## WINDOW$display()
                mask.onset = CLOCK$time
                scale.rt = scale.value = -1
                mp = 666
                state = 'mask-present'
            }
        }, 'mask-present' = {
            if((CLOCK$time - mask.onset) > LONELY.MASK.DURATION)state = 'show-leftright'
        }, 'show-leftright' = {
            WINDOW$clear(c(.5, .5, .5))
            ## WINDOW$draw(m) ##! Bez maski
            TXT$set.string("LEWO     PRAWO")
            center(TXT, WINDOW)
            TXT$set.position(c(WINDOW$get.size()[1] / 2, WINDOW$get.size()[2] * scale.position))
            WINDOW$draw(TXT)
            WINDOW$display()
            leftright.onset = CLOCK$time
            state = 'measure-reaction'
        }, 'measure-reaction' = {
            if(any(BUTTON.PRESSED[1:2] > stim.onset) || ((CLOCK$time - leftright.onset) > MAX.REACTION.TIME)){
                response = which(BUTTON.PRESSED[1:2] > stim.onset)
                rt = BUTTON.PRESSED[response] - stim.onset
                acc = as.numeric(response == c(left = 1, right = 2)[side])
                if(withscale == 1){
                    scale.onset = CLOCK$time
                    state = 'draw-scale'
                }else{
                    if(feedback == 1){
                        feedback.onset = CLOCK$time
                        state = 'feedback'
                    }else{
                        state = 'done'
                    }
                }
            }
        }, 'draw-scale' = {
            WINDOW$set.mouse.cursor.visible(T)
            if(((CLOCK$time - scale.onset) > SCALE.MAX.DURATION) ||
               (BUTTON.PRESSED[1] > scale.onset)){
                    scale.rt = BUTTON.PRESSED[1] - scale.onset
                    scale.value = mp[1]
                    state = 'done'
            }else{
                WINDOW$clear(c(.5, .5, .5))
                WINDOW$draw(m)
                switch(as.character(scale),
                       'pas' = {
                           mp = draw.scale(list(M = c('Nic nie widziałem', 'Widziałem niewyraźnie', 'Widziałem dość wyraźnie', 'Widziałem bardzo wyraźnie'),
                                                K = c('Nic nie widziałam', 'Widziałam niewyraźnie', 'Widziałam dość wyraźnie', 'Widziałam bardzo wyraźnie'))[[USER.DATA$gender]],
                                           background.color = c(.5, .5, .5), position = scale.position, draw.bar = F)
                       },
                       'cpas' = {
                           mp = draw.scale(list(M = c('Nic nie widziałem', 'Widziałem niewyraźnie', 'Widziałem dość wyraźnie', 'Widziałem bardzo wyraźnie'),
                                                K = c('Nic nie widziałam', 'Widziałam niewyraźnie', 'Widziałam dość wyraźnie', 'Widziałam bardzo wyraźnie'))[[USER.DATA$gender]],
                                           background.color = c(.5, .5, .5), position = scale.position, draw.bar = T)
                       },
                       'ias' = {
                           mp = draw.scale(rep("", 11), gradient  = T,
                                           background.color = c(.5, .5, .5), position = scale.position, draw.bar = F)
                       },
                       'cs' = {
                           mp = draw.scale(list(M = c('Nic nie widziałem', 'Widziałem bardzo wyraźnie'),
                                                K = c('Nic nie widziałam', 'Widziałam bardzo wyraźnie'))[[USER.DATA$gender]],
                                           background.color = c(.5, .5, .5), position = scale.position, draw.bar = T)
                       })
                WINDOW$display()
            }
        }, 'feedback' = {
            if((CLOCK$time - feedback.onset) < FEEDBACK.TIME){
                WINDOW$clear(c(.5, .5, .5))
                TXT$set.string(c('Źle', 'Dobrze', 'Za późno')[ifelse(rt > MAX.REACTION.TIME, 3, acc + 1)])
                WINDOW$draw(center.win(TXT))
                WINDOW$display()
            }else{
                state = 'done'
            }
        }, 'done' = {
            WINDOW$clear(c(.5, .5, .5))
            WINDOW$display()
            ## Zapisujemy dotychczasowe dane
            dane[trial, c('contrast', 'acc')] = c(contrast, acc)
            if(trial > FITTING.START){
                model <<- glm(acc ~ contrast, dane[1:trial,], family = binomial)
                new.contrast <<- optimize(function(x)(TARGET.ACC - binomial()$linkinv(coef(model)[1] + x * coef(model)[2])),
                                        c(0, 1))
            }
            return(list(scalert = scale.rt, scalevalue = scale.value,
                        rt = rt, acc = acc, contrastopt = new.contrast))
        })
    }
}

docx.instr = function(file, ask.if.done = T){
    res = 'Tak'
    while(res == 'Tak'){
        system(sprintf('libreoffice --view --invisible %s', file))
        if(ask.if.done){
            res = gui.show.instruction('Czy chcesz jeszcze raz zobaczyć ostatnią instrukcję?', c('Nie', 'Tak'))
        }else{ break }
    }
}

TASK.NAME <<- 'gabortms'

cnd = db.random.condition(c('pas', 'cpas', 'ias', 'cs'))

gui.user.data()

run.trials(trial.code, condition = cnd, expand.grid(side = c('left', 'right'), contrast = c(.1, .4), scale = cnd,
                                                    withscale = 0, feedback = 0, duration = 32),
                                        b = 10, n = 5, record.session = T)

if(!interactive())quit("no")
