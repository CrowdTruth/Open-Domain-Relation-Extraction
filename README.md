# CrowdTruth Corpus for Open Domain Relation Extraction

This repository contains a ground truth corpus for open domain relation extraction from sentences, acquired with crowdsourcing and processed with **[CrowdTruth](http://crowdtruth.org/)** metrics that capture ambiguity in annotations by measuring inter-annotator disagreement.

The dataset contains annotations for 4,100 sentences sampled from Angeli et al. (1) and Riedel et al. (2), over 16 relations, with each sentence annotated by 15 workers. The sentences have been pre-processed with Distant Supervision (3) using the Freebase knowledge base, in order to identify the term pairs in each sentence that are likely to express a relation. The crowdsourced data was collected from [Figure Eight](http://figure-eight.com/) and [Amazon Mechanical Turk](https://mturk.com/).

This corpus has been discussed in the following papers:

* Anca Dumitrache, Lora Aroyo and Chris Welty: **[Crowdsourcing Semantic Label Propagation in Relation Classification](https://arxiv.org/abs/1809.00537)**. [FEVER](http://fever.ai/) Workshop at [EMNLP 2018](http://emnlp2018.org/).
* Anca Dumitrache, Lora Aroyo and Chris Welty: **[False Positive and Cross-relation Signals in Distant Supervision Data](https://arxiv.org/abs/1711.05186)**. [AKBC](http://www.akbc.ws/) Workshop at [NIPS 2017](http://nips.cc/).
* Anca Dumitrache, Lora Aroyo and Chris Welty: **[Disagreement in Crowdsourcing and Active Learning for Better Distant Supervision Quality](http://crowdtruth.org/wp-content/uploads/2017/03/collint17-open-domain.pdf)**. [Collective Intelligence 2017](http://collectiveintelligenceconference.org/).

Sentence-level data is available in file:
``` |--data/output/aggregated_sentences.csv ``` 

Worker-level data is available in file:
``` |--data/output/aggregated_workers.csv ``` 

Raw crowdsourcig data is available in folder:
``` |--data/input/ ```

Results of the relation classification model are available in folder:
``` |--data/model_results/ ```

### References

(1) Angeli, Gabor, et al. "Combining distant and partial supervision for relation extraction." Proceedings of the 2014 conference on empirical methods in natural language processing (EMNLP). 2014.

(2) Riedel, Sebastian, et al. "Relation extraction with matrix factorization and universal schemas." Proceedings of the 2013 Conference of the North American Chapter of the Association for Computational Linguistics (NAACL). 2013.

(3) Mintz, Mike, et al. "Distant supervision for relation extraction without labeled data." Proceedings of the Joint Conference of the 47th Annual Meeting of the ACL and the 4th International Joint Conference on Natural Language Processing of the AFNLP: Volume 2-Volume 2. Association for Computational Linguistics, 2009.
