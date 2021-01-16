(let ([x (read)])
  (if (eq? (land x 1) 0)
      (lor x 1)
      (lxor x 1)))
