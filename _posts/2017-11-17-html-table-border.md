---
layout:     post
title:      "[TIL] HTML 테이블과 보더"
date:       2017-11-17 11:21:00
summary:    "[TIL] HTLM 테이블과 테이블 보더에 대해 정리"
categories: TIL HTML TABLE BORDER
---

## HTML 테이블 보더

대충 다음과 같다. 테이블의 보더와 `<td>`, `<th>`의 보더가 별개라는 점!

(표준은 [W3C표준]`https://www.w3.org/TR/CSS21/tables.html`) 참조할 것)

```html
<table>
  <tr>
    <th>First name</th>
    <th>Last name</th>
  </tr>
  <tr>
    <td>John</td>
    <td>Doe</td>
  </tr>
  <tr>
    <td>Jane</td>
    <td>Doe</td>
  </tr>
</table>
```

```css
table
{
//border-collapse: collapse;
border-spacing: 2px;
border: 2px dashed red;
}

td
{
padding: 5px;
border: 2px solid blue;
}

th
{
padding: 10px;
border: 5px dotted red;
}
```

[JS피들](https://jsfiddle.net/enshahar/nLrdvgcx/)에서 렌더링 결과를 볼 수 있다.

보더 스타일이나 보더 두께 등은 하위 엘리먼트에 상속되지 않는다.
