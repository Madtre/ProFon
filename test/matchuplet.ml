let f u = match u with
|a::c::[],b->c
|_->0
in f ([1;4],2)