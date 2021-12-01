program nanpure
  implicit none 
  integer :: i=0,j=0,k,l, a(9,9)=0
  logical :: fg=.true. , a0(9,9)=.false.

  ! 入力
  do i = 1, 9
    do j = 1, 9
      read *, a(j,i)
      if(a(j,i) == 0)a0(j,i) = .true.
    end do
  end do
  i=1
  j=1
  

  ! (j,i)が(9,9)になるまで処理を続ける
  do while(i /= 9 .and. j /= 9)

    ! 数字フェーズ
    100 if(a0(j,i))then
      a(j,i)=a(j,i)+1
      
      ! 衝突調査
      do k = 1,9
        if(i /= k .and. a(j,i)==a(j,k))then
          fg =.false.
        end if
        if(j /= k .and. a(j,i)==a(k,i))then
          fg = .false.
        end if
      end do
      do k = 1,9
        do l = 1,9
          if((i /= k .or. j /= l) .and. (i-1)/3 == (k-1)/3 .and. (j-1)/3 == (l-1)/3 .and. a(j,i)==a(l,k))then
          fg = .false.
          end if
        end do
      end do
      
      ! 次の処理の分岐
      if(fg)then
        goto 200
      else if(a(j,i)==9)then
        goto 200
      else
        fg = .true. ! 初期化
        goto 100
      end if
    end if
  
    ! 移動フェーズ fgによって前に進むか戻るか決める
    200 do
      if(fg)then
        if(j==9 .and. i ==9)then
          goto 300
        else if(j==9)then
          i=i+1
          j=1
        else
          j=j+1
        end if
      else
        if(a0(j,i))then
          a(j,i)=0
        end if
        if(j==1 .and. i==1)then
          stop "error"
        else if(j==1)then
          i=i-1
          j=9
        else
          j=j-1
        end if
      end if

      ! 次の処理の分岐
      if(a0(j,i) .and. a(j,i) /= 9)then
        goto 100
        fg = .true. ! 初期化
      else
        goto 200
      end if
    end do

  end do
  ! プリント
  300 do i=1,9
    do j=1,9
      if(j/=9)then 
        print '(I4$)',a(j,i)
      else 
        print  '(I4)',a(j,i)
      end if
    end do
  end do

end program nanpure
