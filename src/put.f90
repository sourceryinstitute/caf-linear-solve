module coarray_fortran
  implicit none

  real :: sr_loc%double_mat_recvdata(:,:)[:]

contains

  subroutine lmpi_complete_xfer(dq,sr_opt=sr(color))
    real, intent(in) :: dq(:,:)
    type(...) :: sr_opt
    sync all
  end subroutine

  subroutine lmpi_start_xfr(dq,sr_opt=sr(color))
    real, intent(in) :: dq(:,:)
    type(...) :: sr_opt

    allocate( sr_loc%double_mat_recvdata(M,N)[*] )

    ! Put data on image i_other_proc

    !  rlb = row lower bound
    !  rub = row upper bound
    !  clb = column lower bound
    !  cub = column upper bound

    sr_loc%double_mat_senddata(...) =  dq(...)

    associate(length_recipient=>length[i_other_proc+1])

    associate(rlb_provider=>1              )
    associate(rub_provider=>length_recipient*nallocdim)

    associate(clb_provider=>sr_loc%sendproc(i_other_proc+1))
    associate(cub_provider=>sr_loc%sendproc(i_other_proc+1)+length_recipient*nallocdim-1)

    associate(rlb_recipient=>1              )
    associate(clb_recipient=>sr_loc%recvproc(i_other_proc+1)[i_other_proc+1])

    associate(rub_recipient=>length_recipient[i_other_proc+1]*nallocdim)
    associate(cub_recipient=>sr_loc%recvproc(i_other_proc+1)[i_other_proc+1] + length_recipient*nallocdim-1)

      sr_loc%double_mat_recvdata(rlb_recipient:rub_recipient,clb_recipient:cub_recipient)[i_other_proc+1] &
    = sr_loc%double_mat_senddata(rlb_provider :rub_provider ,clb_provider :cub_provider )

    end associate
    end associate

    end associate
    end associate

    end associate
    end associate

    end associate
    end associate

    end associate
  end subroutine lmpi_start_xfr()

end module
