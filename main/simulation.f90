program simulation
    use random
    use omp_lib
    
    !shortcuts to get values for different teams
    integer, parameter :: qatar =  1, ecuador =  2, senegal =  3, netherlands =  4
    integer, parameter :: england =  5, iran =  6, united_states =  7, ukraine =  8
    integer, parameter :: argentina =  9, saudi_arabia =  10, mexico =  11, poland =  12
    integer, parameter :: france =  13, australia =  14, denmark =  15, tunisia =  16
    integer, parameter :: spain =  17, costa_rica =  18, germany =  19, japan =  20
    integer, parameter :: belgium =  21, canada =  22, morocco =  23, croatia =  24
    integer, parameter :: brazil =  25, serbia =  26, switzerland =  27, cameroon =  28
    integer, parameter :: portugal =  29, ghana =  30, uruguay =  31, korea_republic =  32
    integer, dimension(8,4) :: groups
    integer :: results(0:9999)
    integer :: repetition, goalsA_, goalsB_, win_

    
    !variables del programa
    real, dimension(32,4) :: data
    groups(1, :) = (/qatar, ecuador, senegal, netherlands/)
    groups(2, :) = (/england, iran, united_states, ukraine/)
    groups(3, :) = (/argentina, saudi_arabia, mexico, poland/)
    groups(4, :) = (/france, australia, denmark, tunisia/)
    groups(5, :) = (/spain,  costa_rica, germany, japan/)
    groups(6, :) = (/belgium, canada, morocco, croatia/)
    groups(7, :) = (/brazil, serbia, switzerland, cameroon/)
    groups(8, :) = (/portugal, ghana, uruguay, korea_republic/)

    call omp_set_num_threads(10000)

    !inicializar los datos de los equipos
    call read_data(data, "/home/ignacio/Master/Programación_avanzada/Proyecto/Data/datos.dat")

    !$OMP PARALLEL
    call wc_complete(results(omp_get_thread_num()))
    !$OMP END PARALLEL

    open(100, file = "/home/ignacio/Master/Programación_avanzada/Proyecto/main/results", status = "old", action = "write")
    do repetition = 0, 9999
        write(100,*) results(repetition)
    end do
    close(100)
    
    
    ! call match(data(belgium,:),data(serbia,:),goalsA_, goalsB_, win_)
    contains

    subroutine  wc_complete(champion)
        integer, dimension(8,2) :: qualified_groups, matchups_16
        integer, allocatable, dimension(:,:) :: matchups_8, matchups_4, final
        integer :: primer, segundo, idx, discard_1,  discard_2
        real :: tie_braker
        integer, intent(out) :: champion

        !$OMP PARALLEL DO
        do idx = 1, 8
            call group(groups(idx,:), primer, segundo)
            qualified_groups(idx, 1) = primer
            qualified_groups(idx, 2) = segundo
        end do
        !$OMP END PARALLEL DO
        do idx = 1, 8
            matchups_16(idx, 1) = qualified_groups(idx, 1)
            matchups_16(idx, 2) = qualified_groups(9 - idx, 2)
        end do

        call round(matchups_16, matchups_8)
        call round(matchups_8, matchups_4)
        call round(matchups_4, final)
        call match(data(final(1,1), :), data(final(1,2), :), discard_1, discard_2, champion)
        call random_number(tie_braker)
        if (champion .eq. 0) champion = floor(tie_braker * 2) + 1
        champion = final(1, champion)


    end subroutine

    subroutine round(matchups, next_matchups)
        integer, dimension(:,:), intent(in) :: matchups
        integer, allocatable, dimension(:,:) :: next_matchups
        integer, allocatable, dimension(:) :: winners
        integer, dimension(2) :: matchups_shape
        integer :: i, winner, discard1, discard2
        real :: tiebraker

        
        matchups_shape = shape(matchups)
        allocate(winners(matchups_shape(1)))
        allocate(next_matchups(matchups_shape(1)/2, 2))
        do i = 1, matchups_shape(1)
            call match(data(matchups(i, 1), :), data(matchups(i, 2), :), discard1, discard2, winner)
            call random_number(tiebraker)
            if (winner .eq. 0) winner = floor(tiebraker * 2) + 1
            winners(i) = matchups(i, winner)
        end do

        do i = 1, matchups_shape(1)/2
            next_matchups(i, 1) = winners((2*i) - 1)
            next_matchups(i, 2) = winners(2*i)
        end do
    end subroutine

    subroutine group(group_teams, first, second)
        integer, dimension(4), intent(in) :: group_teams
        integer, dimension(6,2) :: matches
        integer, dimension(:,:), allocatable :: table
        integer, intent(out) :: first, second
        integer :: indx, goalsA, goalsB, match_winner

        allocate(table(group_teams(1):group_teams(4), 3))
        matches(1,:) = (/group_teams(1), group_teams(2)/)
        matches(2,:) = (/group_teams(1), group_teams(3)/)
        matches(3,:) = (/group_teams(1), group_teams(4)/)
        matches(4,:) = (/group_teams(2), group_teams(3)/)
        matches(5,:) = (/group_teams(2), group_teams(4)/)
        matches(6,:) = (/group_teams(3), group_teams(4)/)

        table = 0
        do indx = 1, 6
            call match(data(matches(indx, 1), :), data(matches(indx, 2), :), goalsA, goalsB, match_winner)
            table(matches(indx, 1), 2) = table(matches(indx, 1), 2) + (goalsA - goalsB)
            table(matches(indx, 2), 2) = table(matches(indx, 2), 2) + (goalsB - goalsA)
            table(matches(indx, 1), 3) = table(matches(indx, 1), 3) + goalsA
            table(matches(indx, 2), 3) = table(matches(indx, 2), 3) + goalsB

            if (match_winner .eq. 0) then
                table(matches(indx, 1), 1) = table(matches(indx, 1), 1) + 1
                table(matches(indx, 2), 1) = table(matches(indx, 2), 1) + 1
            else
                table(matches(indx, match_winner), 1) = table(matches(indx, match_winner), 1) + 3
            end if
        end do
        ! write(*,*) '----'
        ! do indx = group_teams(1), group_teams(4)
        !     write(*,*) table(indx,1), table(indx,2), table(indx, 3)
        ! end do
        ! write(*,*) '----'

        call find_top_two(table, group_teams(1), group_teams(4), first, second)
    end subroutine group

    subroutine find_top_two(table, first_element_group, last_element_group, first_place, second_place)
        integer, dimension(4,3) :: table
        integer, dimension(4) :: position, dummy_values, dummy_prov, dummy_position
        integer, allocatable, dimension(:,:) :: table_prov
        integer, intent(in) :: first_element_group, last_element_group
        integer, intent(out) :: first_place, second_place
        integer :: i, idx_highest

        allocate(table_prov(first_element_group:last_element_group, 3))

        do i = 1, 4
            table_prov(first_element_group + (i - 1),:) = table(i, :)
        end do

        do i = 1, 4
            position(i) = first_element_group + (i - 1)
        end do


        do i = 1, 4
            dummy_values(i) = (table_prov(first_element_group + (i - 1), 1) * 1000) + &
                              (table_prov(first_element_group + (i - 1), 2) * 100) + &
                              (table_prov(first_element_group + (i - 1), 3) * 10)
        end do

        do i = 1,3
            dummy_position = position
            dummy_prov = dummy_values
            idx_highest = maxloc(dummy_values(i:4), DIM = 1)
            dummy_prov(i) = dummy_values(i + (idx_highest - 1))
            dummy_position(i) = position(i + (idx_highest - 1))
            dummy_prov(i + (idx_highest - 1)) = dummy_values(i)
            dummy_position(i + (idx_highest - 1)) = position(i)
            dummy_values = dummy_prov
            position = dummy_position
        end do

        ! do i = first_element_group, last_element_group
        !     write(*,*) table_prov(i, :)
        ! end do
        ! write(*,*) ''
        ! write(*,*) position
        ! write(*,*) dummy_values
        first_place = position(1)
        second_place = position(2)

    end subroutine

    subroutine match(teamA, teamB, teamA_goals, teamB_goals, winner)
        real, dimension(4), intent(in) :: teamA, teamB
        integer, intent(out) :: teamA_goals, teamB_goals, winner
        integer :: actions = 16, att = 1, def = 2
        real, dimension(:,:), allocatable :: OV, MV, DV, GV
        integer :: indx, start_zone = 2
        logical :: is_goal = .FALSE.

        allocate(OV(2, actions))
        allocate(MV(2, actions))
        allocate(DV(2, actions))
        allocate(GV(2, actions))

        !initialize values
        call gen_decay(teamA(1), 0.05, actions, GV(1,  1:actions))
        call gen_decay(teamB(1), 0.05, actions, GV(2,  1:actions))
        call gen_decay(teamA(2), 0.05, actions, DV(1,  1:actions))
        call gen_decay(teamB(2), 0.05, actions, DV(2,  1:actions))
        call gen_decay(teamA(3), 0.05, actions, MV(1,  1:actions))
        call gen_decay(teamB(3), 0.05, actions, MV(2,  1:actions))
        call gen_decay(teamA(4), 0.05, actions, OV(1,  1:actions))
        call gen_decay(teamB(4), 0.05, actions, OV(2,  1:actions))

        teamA_goals = 0
        teamB_goals = 0

        !main values
        do indx = 1, actions
            call action(DV(att, indx), MV(att, indx), OV(att, indx), GV(def, indx), &
            DV(def, indx), MV(def, indx), OV(def, indx), is_goal, start_zone)
            if (is_goal)  then
                if (att == 1) then
                    teamA_goals = teamA_goals + 1
                else
                    teamB_goals = teamB_goals + 1
                end if
            end if
            call change_posetion(att, def)
        end do

        if (teamA_goals .gt. teamB_goals) then
            winner = 1
        else if (teamA_goals .lt. teamB_goals) then
            winner = 2
        else
            winner = 0
        end if

        ! write(*,*) winner, teamA_goals, teamB_goals

    end subroutine

    !ONE ACTION
    subroutine action(DEF_att, MID_att, OFF_att, GK_def, DEF_def, MID_def, OFF_def, GOAL, action_start)
        real, intent(in) :: DEF_att, MID_att, OFF_att, GK_def, DEF_def, MID_def, OFF_def
        logical, intent(out) :: GOAL
        logical, dimension(4) :: battle_results
        integer  ::  action_start, i
        real :: discard

        discard = random_beta(5., 1.25,  .TRUE.)

        battle_results(1) = random_beta(5., 1.25,  .FALSE.) * DEF_att > &
                            random_beta(5., 1.25,  .FALSE.)* OFF_def
        battle_results(2) = random_beta(5., 1.25,  .FALSE.) * MID_att > &
                            random_beta(5., 1.25,  .FALSE.) * MID_def
        battle_results(3) = random_beta(5., 1.25,  .FALSE.) * OFF_att > &
                            random_beta(5., 1.25,  .FALSE.) * DEF_def
        battle_results(4) = random_beta(5., 1.25,  .FALSE.) * OFF_att > &
                            random_beta(5., 1.25,  .FALSE.) * GK_def

        GOAL = .FALSE.
        battle_loop:do i = action_start, 4
            if (.not. battle_results(i)) then
                action_start = 4 - i
                exit  battle_loop
            else if (i == 4) then
                GOAL = .TRUE.
                action_start = 2
            end if
        end do battle_loop

        if (action_start .eq. 0) then
            action_start = 1
        end if

    end subroutine

    subroutine gen_decay(init, rate, n, vector)
        real, intent(in) :: init, rate
        integer, intent(in) :: n
        integer :: i
        real, dimension(n), intent(out) :: vector
        real :: prov

        prov = init
        do i = 1, n
            vector(i) = prov
            prov = prov * (1 - rate)
        end do
    end subroutine

    subroutine change_posetion(att, def)
        integer :: att, def
        att = 3 - att
        def = 3 - def
        
    end subroutine

    subroutine read_data(M, file_name)
        integer :: i
        real, dimension(32,4), intent(out):: M
        character (len=*), intent(in) :: file_name
        open(1, file = file_name, status = "old", action = "read")
        do i = 1, 32
            read(1,*) M(i,1), M(i,2), M(i,3), M(i,4)
        end do
        close(1)
    end subroutine

end program simulation