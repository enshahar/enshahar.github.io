---
layout:     post
title:      Github Page Blog Installation
date:       2017-11-14 14:15:14
summary:    윈도우에 지킬을 사용해 깃헙 페이지 블로그 설치하기
categories: 블로그 jekyll pixyll
---

# 윈도우 사용자의 깃헙 페이지 블로그 설치기

블로깅을 꾸준히 하고 싶은 마음은 있었지만, 게으른 천성으로 인해 HTML 포매팅을 한다던지 하는건 절대 싫고 가능하면 코드와 텍스트를 날것 그대로 쓰고 싶었다. 깃헙 페이지 블로그에 마크다운으로 블로깅을 하려고 생각은 진작 하고 있었지만 게으름으로 인해... 

깃헙 페이이지를 그냥 써도 되겠지만 대세를 따라 지킬을 사용하기로 했다. 자 지킬을 깔아보자!

## 지킬을 깔자

[지킬 홈피](https://jekyllrb.com)에 들어갔다. 지킬 홈피 초기 화면에 보니 빠른 설치에 `gem`이 필요하단다. 십년만에 루비시스템이군. 그럼 일단 루비부터 깔아야 하는건가? 난 회사 사정상 윈도우 7 사용자다. 예전 기억으로 윈도즈에서 지킬 설지가 뭔가 불편했던 것 같다. [지킬 온 윈도우즈](https://jekyllrb.com/docs/windows/)로 들어가본다. "루비 인스톨러를 사용한 설치"를 따라해보자. 루비 인스톨러가 필요하다.

### 루비부터 깔자

[루비 인스톨러 홈페이지](https://rubyinstaller.org/)에 들어간다. 빨간 다운로드 버튼이 보인다. 과감히 클릭한다!

![루비 인스톨러 다운로드]({{ site.url }}/images/rubyInstaller.png)

잘 모르니 최신판으로 Ruby 2.4.2-2 x64를 클릭하자.

설치해보면 MSYS2가 필요하단다. 잘 모르겠으니 엔터를 누르자.

![루비인스톨러 MSYST2]({{ site.url }}/images/rubyInstallerMsysCheck.png)

열심히 기도하다 보면 설치가 끝난다. 네트워크 상황에 따라 걸리는 시간은 다르다. 문제가 생겼다고? 나도 잘 모른다. 스택오버플로우나 구글에게 물어보라.

### 드디어 지킬을 깔아보자!

루비를 설치했다면 루비 패키지 관리 프로그램인 `gem`도 설치됐을 것이다. `gem`으로 지킬을 깔자.

```
gem install jekyll bundler
```

설치가 다 됐다면 한번 지킬을 실행해 본다.

```
C:\Users\Hyunsok> jekyll -v
jekyll 3.6.2
```

## 깃헙 페이지를 만들자

깃헙 페이지에서 블로그를 사용하려면 자신의 깃헙에 *계정이름.github.io*라는 리포지터리를 만들어야 한다. 깃헙 사용법은 따로 적지 않겠다. 

만들고 나면 다음과 같은 절차를 거쳐서 로컬 컴퓨터에 블로그용 깃헙 리포를 만들고 원격과 연결한다. 난 깃 사용시 *cygwin*의 깃을 사용하기 때문에 다음과 같이 했지만 윈도우 깃 사용자는 GUI를 쓰던 명령줄에서 하던 기본적인 절차는 같다.

```
$ cd /cygdrive/e/blog/enshahar.github.io
$ git init
$ cat > index.html
Hello World!
$ git add *
$ git commit -m "initial"
(메시지 생략)
$ git remote add origin https://enshahar@github.com/enshahar/enshahar.github.io.git
$ git push -u origin master
Password for 'https://enshahar@github.com':
(메시지 생략)
```

이제 잘 되나 브라우저에서 `https://enshahar.github.io/`를 열어보자.

![Hello World 인덱스페이지]({{ site.url }}/images/helloworld.png)

좋다. 이제 본격적으로 지킬을 사용해 홈피를 구축할 때다.

## 지킬 테마를 찾자

[지킬 테마 페이지](https://github.com/jekyll/jekyll/wiki/Themes)에 가보면 엄청난 양의 지킬 테마를 볼 수 있다. 난 그 중에서 john Otander의 [pixyll](https://github.com/johnotander/pixyll)을 택했다.

포크후 설치하는 방법도 있지만 리포를 덮어쓰는 방법을 택하기로 했다. pixyll 깃 페이지에서 "Clone or Download" 버튼을 눌러 소스를 다운로드한다. 

![다운로드]({{ site.url }}/images/pixyll_download.png)

블로그 디렉터리의 모든 파일을 지우고 다운로드한 소스 파일을 블로그 디렉터리에 복사한다.

디렉터리에는 다음과 같은 파일이 생긴다.

![복사후 디렉터리 내용]({{ site.url }}/images/afterPixyllCopy.png)

### `_config.yml` 설정

`_config.yml` 설정을 적당히 바꾼다.

```yaml
# Site settings
title: 사이트 타이틀
email: 전자우편 주소
author: 이름
description: "설명"
baseurl: ""
url: "블로그 URL"

# Build settings
markdown: kramdown
permalink: pretty
paginate: 5
```

### 로컬에서 실행

지킬을 실행해 로컬 서버를 돌리면서 블로그를 미리 살펴볼 수 있다. `--watch`를 설정하면 소스코드 변경시 자동으로 html이 갱신된다.

```
$ jekyll serve --watch
```


혹시 윈도우즈에서 `jekyll serve --watch`가 오류를 낸다면 `bundle install`이나 `bundle update`로 번들을 업데이트한 다음에 `bundle exec jekyll serve --watch`로 실행하라.

`http://localhost:4000/`를 브라우저에서 열면 다음과 같은 페이지를 볼 수 있다.

![]({{ site.url }}/images/pixyll_initial.png)


## 자신의 블로그에 맞춰 바꿀 부분 바꾸고 첫번째 포스팅 올리기

(이 내용은 pixyll에만 해당하는 부분이 있을 수 있다.)

- `about.md`의 헤더에 있는 yaml에서 `About pixyll` 제목을 바꾸자.
- `contact.md`의 헤더에 있는 제목을 바꾸자.

### 첫번째 포스팅 작성하기

마크다운으로 포스팅을 작성한다. 현재 여러분이 보고 있는 이 포스트가 바로 내 첫번째 포스트다. 포스트 위치는 `_posts` 아래이다.

맨 앞부분엔 헤더를 둬야 한다(두지 않아도 관계 없지만 제목 등을 지정하려면 헤더를 두는게 낫다).

```yaml
---
layout:     post
title:      test
summary:    윈도우에 지킬을 사용해 깃헙 페이지 블로그 설치하기
categories: jekyll pixyll 깃헙
---
```

크게 세가지 문제가 있다.

#### 이미지 삽입

글 안에서 이미지를 사용할 때 문제가 있다. `{{site.url}}/images/이미지이름`으로 이미지를 지정하면 기트허브 페이지나 지킬 서버에서는 정상적으로 화면을 볼 수 있다.

#### `categories`에 한글 카테코리를 넣은 경우의 경로 문제

`categories`에 `jekyll pixyll 깃헙`과 같이 카테고리를 지정하면 파일 경로가 `https://enshahar.github.io/jekyll/pixyll/깃헙/날짜/파일명/`처럼 잡히는데, 중간에 있는 한글 경로를 로컬 지킬 서버가 제대로 서비스를 못 해준다. 다행히 깃헙에 `push`하고 나서 보면 깃헙 페이지에서는 제대로 보이므로 한글 카테고리를 지정한 경우 확인이 필요하면 깃헙 페이지에 올려서 확인하라.

#### 한글폰트

디폴트 설정대로하면 그냥 명조체를 사용한다. 정말 구리다. 바꾸자! 헤더에 폰트 경로를 넣자. 그냥 `index.html`의 `<!-- fonts -->` 바로 밑에 원하는 폰트를 넣어라. 나는 나눔고딕을 넣었다.

```html
<!-- Fonts -->
<link rel="stylesheet" href="https://fonts.googleapis.com/earlyaccess/nanumgothic.css">
```

페이지에서 폰트를 표시하는 부분의 폰트 클래스를 바꿔야 한다. `_sass/variables.scss`를 보면 폰트 패밀리 지정이 있다.

```scss
// Typography
$base-font-size: 14px !default;
$bold-font-weight: bold !default;
$font-family: 'Nanum Gothic', 'Merriweather', 'PT Serif', Georgia, 'Times New Roman', serif !default;
$line-height: 1.5 !default;
$heading-font-family: 'Lato', 'Helvetica Neue', Helvetica, sans-serif !default; $heading-font-weight: 900 !default;
$heading-line-height: 1.25 !default;
```

몇가지 변경하고 싶은 부분이 있지만 일단 오늘은 여기까지로 마치자.

