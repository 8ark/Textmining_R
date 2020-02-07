# Textmining_R
> 
# 국회회의록 문서를 통한 텍스트마이닝
> 텍스트 데이터를 통한 정당 분류기 구축

[![NPM Version][npm-image]][npm-url]
[![Build Status][travis-image]][travis-url]
[![Downloads Stats][npm-downloads]][npm-url]

국회의원들의 발언 데이터를 통한 분류모델 작성 과정이다. 연세대학교 송 민 교수가 2012년에 작성한 '텍스트 마이닝을 활용한 신문사에 따른 내용 및 논조 차이점 분석'의 프로세스를 동일하게 따라갔다. 이번 모델은 '패스트트랙'과 관련한 정당의 발언을 기준으로 작성했다. 논란이 있던 지난 기간의 법제사법위원회 회의록을 토대로 전처리, 모델링, 분류 등의 과정을 거쳤다.

## 국회회의록 데이터

![](/image/국회회의록.png)

데이터 출처:
국회회의록 사이트 내에서 문서화된 회의록 데이터를 다운로드 하였음. 특정 이슈를 고려해 해당 기간 내의 회의록만 다운로드. 법제사법위원회의 10회, 11회 회의록을 다운로드.

## 전처리
![](/image/전처리.png)

전처리 과정:
PDF 파일은 전처리에 어려움이 있어 HWP로 다운로드 한 파일을 txt로 재저장한 파일을 시작데이터로 사용. 국회회의록 사이트 내에서 문서화된 회의록 데이터를 다운로드. 특정 이슈를 고려해 해당 기간 내의 회의록만 다운로드. PDF 파일은 전처리에 어려움이 있어 HWP로 다운로드 한 파일을 txt로 재저장한 파일을 분석 데이터로 사용.


## 시각화

![](/image/시각화_1.png)

시각화 관련_1:
각 정당 별 주요 단어를 정렬. 그래프로 보기좋게 나열
더불어민주당은 주로 '문제', '자유한국당', '방해' 등의 단어가 있는 것으로 보아 '자유한국당이 방해하고 문제'라고 보는 듯하다.
자유한국당은 '오늘', '우리', '때문', '오신환' 등으로 보아 '오늘 회의가 우리 때문에 안 되는 거냐'와 '오신환 의원한테 발언권 달라' 는 내용을 많이 언급한 듯하다.
바른미래당은 '발언권', '불법', '존경' 등으로 보아 '발언권을 달라'(오신환 의원이 사보임 때문에 발언권 잃었음)는 것과 '패스트트랙이 불법'이라는 것을 어필한 듯 하다.
마지막으로 정의-민주평화(교섭단체, 실질적으로 이 회의에서는 민주평화만 있었음)당은 특색이 없다. 발언 데이터도 3문단만 있다. 어느 당에도 눈총 안 받고 끝내고 싶었던 걸까.

![](/image/시각화_2.png)

워드클라우드:
위 그래프를 워드클라우드로 바꿔보았다. 정작 보기는 어렵다.