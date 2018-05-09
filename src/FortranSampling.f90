! Program SampleNet.f90

! 1 - Reads a network from file
!
! 2 - Constructs subnetworks by sampling nodes:
!  (a) - Samples m 'key' nodes: randomly, according to degree, abundances (see (d)) or module
!  (b) - Adds to the basic nodes some of their first neighbors:
!        at most nfn (a parameter)  or
!        a fraction anfn of its neighbors
!  (c) - Neighbors are added: randomly or according to weights assigned to
!        the links following an exponential distribution
!  (d) - Abundances can be assigned from a lognormal, Fisher or exponential distributions
!        In this version the distributions are assigned per module
!
! 3 - For each sub-network, consisting of the key nodes, the added neighbors and
!     the links between the key nodes and their added neighbors, finds all connected
!     components and calculates the size of largest component
!
! Calls the following subroutines
!
! FINDNEIGHBORS (a,k,n,v,m)
! Finds the neighbors of node k in the n x n adjacency matrix a
! m = number of neighbors
! v = vector containing the neighbors
!
! CLUSTERS (a,n,maxsize,icount)
! Finds all connected components of the adjacency matrix a
! icount = number of connected components
! maxsize = size of largest component
! csizes = contains the sizes of all clusters
!  found and can be printed if a histogram of cluster sizes is needed.

! FINDTREE (a,vm,i,n,nvm)
! Finds the component (tree of connections) of node i
! This is a recursive routine, that calls itself and findneighbors

! FISHERLOG
! generates a Fisher log-series distribution

! LOGNORMAL
! generates a log-normal distribution

! NUMBSTR(I,N,S)
! transform number N into a string S of size ID

! SAMPLING_CRITERION(icrit)
! computed the probability of sampling nodes according to specified criterion


! Parameters of NetSample are:
!   m = number of key nodes to be sampled:
!       mi = initial m
!       mf = final m
!       mstep: m = mi + i*mstep until m > mf
!   nfn = number of first neighbors to be added
!   nr = number of replicas for each m


! Marcus A.M. de Aguiar - 07/apr/2018

! To compile in linux or Mac:
! f2py -c --fcompiler=gnu95 -m FortranSampling FortranSampling.f90


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! module defining global variables
MODULE globalvar
double precision, ALLOCATABLE, SAVE :: key_prob(:)
INTEGER, ALLOCATABLE, SAVE :: a(:,:),jj(:,:)
INTEGER, ALLOCATABLE, SAVE :: a_aux(:,:)
INTEGER, ALLOCATABLE, SAVE :: module_status(:),modsize(:),idx(:)
INTEGER, SAVE :: n,imods,mnew

END MODULE globalvar
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE subsampling(net_in,net_out,crit,key_nodes,anfn,numb_hidden,hidden_modules, &
                       size_n, module_sizes, n_modules, sampled_nodes, sampled_edges)
USE globalvar
IMPLICIT double precision(A-H,O-Z)
integer net_in(*), net_out(*), size_n(*), module_sizes(*), n_modules(*)
integer sampled_nodes(*), sampled_edges(*)
double precision anorm, av_degree, auxw
INTEGER, INTENT(IN), DIMENSION(2) :: crit
INTEGER, INTENT(IN) :: key_nodes
INTEGER, INTENT(IN) :: numb_hidden
INTEGER, INTENT(IN) :: hidden_modules(10)
double precision, INTENT(IN) :: anfn
INTEGER, ALLOCATABLE :: v(:)
INTEGER, ALLOCATABLE :: vk(:),row(:),col(:)
INTEGER, ALLOCATABLE :: degori(:),degsamp(:)
double precision, ALLOCATABLE :: w(:,:),vw(:),vwaux(:),prob_aux(:)
INTEGER :: js(1),hidden,hiddentot
INTEGER mm
mm = 1


icrit = crit(1)
neigh_crit = crit(2)
m = key_nodes
nfn = int(anfn)

k = 0
n = size_n(1)

ALLOCATE (a_aux(n,n))
ALLOCATE (v(n),vk(n))
ALLOCATE (a(n,n))
ALLOCATE (idx(n))
ALLOCATE (w(n,n))

if(neigh_crit == 1) then
!    ALLOCATE (w(n,n))
    w = 0.0
end if

CALL init_random_seed()

k = 0
a = 0


do i=1,n
  do j=1,n
    a(i,j) = net_in(i + (j-1) * n)
    if(neigh_crit == 1) then
        aux = unifrnd()  ! assign weights to links
        auxw = log(1.0D0/aux)      ! following exponential distribution
        w(i,j) = auxw
        w(j,i) = auxw
    end if
  end do
end do



ALLOCATE(degori(n))
degori = sum(a,DIM=1)

ALLOCATE (key_prob(n),prob_aux(n))
key_prob = 0.0D0

imods = n_modules(1)



ALLOCATE(module_status(imods),modsize(imods))
modsize = 0
do i=1,imods
  modsize(i) = module_sizes(i)
end do

module_status = 0
if (numb_hidden /= 0) then
    DO im=1,imods
        DO iml=1,numb_hidden            ! mark modules to be excluded (hidden modules)
            if(im == hidden_modules(iml)) module_status(im) = 1
        END DO
    END DO
end if

! calcule total number of hidden nodes
hiddentot = sum(modsize*module_status)

! average connectivity
icon = SUM(a)
av_degree = dble(icon)/dble(n)

! find connected clusters of the initial network
call clusters(a,n,maxsize,nclusters)

! print basic info on screen
!print *,
!print *, 'network size =',n
!print *, 'average degree =',av_degree
!print *, 'total number of clusters =',nclusters
!print *, 'size of largest cluster =',maxsize
!print *,
!print *, 'hidden modules are      ',(hidden_modules(i),i=1,numb_hidden)
!if(numb_hidden /= 0) then
!    print *, 'number of hidden nodes = ',(modsize(hidden_modules(i)),'  +',i=1,numb_hidden-1), &
!                modsize(hidden_modules(numb_hidden)),'    = ',hiddentot
!end if
!print *,

! Calculate probabilities according to sampling criterion
!
CALL SAMPLING_CRITERION(icrit)
!
! The output of this subroutine is the vector "key_prob" containing the
! cummulative probability that nodes will be sampled.
! For random sampling key_prob(i) = i/n (if no modules are skipped)


if(anfn > 1.0D0) then
    nsize = m*nfn     ! maximum size when all nfn neighbors are added
else
    nsize = INT(5.0*m*av_degree*anfn)  ! estimated size times 5 for safety
end if
a_aux = a
ALLOCATE (row(nsize),col(nsize))  ! index of nodes in subnetwork

v = 0
idx = 0

! try m times to select key nodes from the network and put nodes in v
! actual number of selected nodes is mm and may be smaller than m
do k=1,m
    aux = unifrnd()
    ! sample according to criterion
    prob_aux = key_prob - aux
    js = minloc(prob_aux,MASK=prob_aux.GT.0.0)
    if(k == 1) then
        v(k) = js(1)
        idx(js) = 1
        mm = 1     ! count nodes in subnetwork
    else
        it = 0     !check if node has already been selected
        do l=1,k-1
            if(js(1) == v(l)) it = 1
        end do
        if(it == 0) then   !it=0 means node is new
            mm = mm + 1
            v(mm) = js(1)
            idx(js) = mm
        end if
    end if
end do

! add up to nfn or the fraction anfn of first neighbors to all mm key nodes
vk = 0
mnew = mm
linkc = 0
row = 0
col = 0
do k=1,mm
    ! find neighbors of node v(k) and put them in the vector vk
    ! mk = total number of neighbors = degree of node v(k)
    ! mkk = min[mk,nfn] if nfn > 0
    ! mkk = mk*anfn if nfn = 0
    call findneighbors(a,v(k),n,vk,mk) ! get neighbors
    if(anfn > 1.0) then
        mkk = mk
        if(mk > nfn) mkk = nfn             ! add at most nfn
    else
        mkk = INT(mk*anfn)                 ! add the fraction anfn
    end if
    ! add neighbors randomly
    if(neigh_crit == 0) then
        do l=1,mkk
            aux = unifrnd()
            jsr = int(aux*mk) + 1          ! select random neighbor
            do ll=1,mnew
                if(vk(jsr) == v(ll)) then  ! check if the neighbor has already been added
                    linkc = linkc + 1      ! node is there, just add a link
                    row(linkc) = v(k)
                    col(linkc) = vk(jsr)
                    a_aux(v(k),vk(jsr)) = 2    ! mark link in the matrix
                    a_aux(vk(jsr),v(k)) = 2
                    exit
                end if
            end do
            if(ll == mnew+1) then     ! if neighbor is new
                mnew = mnew + 1       ! subnetwork size increases by 1
                v(mnew) = vk(jsr)     ! neighbor is saved in v and becomes part of the subnetwork
                idx(vk(jsr)) = mnew
                linkc = linkc + 1     ! add link between v(k) and the selected neighbor
                row(linkc) = v(k)
                col(linkc) = vk(jsr)
                a_aux(v(k),vk(jsr)) = 2    ! mark link in the matrix
                a_aux(vk(jsr),v(k)) = 2
            end if
        end do
    else
        ! add neighbors according to chosen criterion
        ! w(i,j) = weight for link i-j according to exponential distribution
        ! vw = vector containing the cummulative weights for the links v(k)-neighbors
        ALLOCATE(vw(mk),vwaux(mk))
        vw = 0.0D0
        vw(1) = w(v(k),vk(1))
        do iik=2,mk
            vw(iik) = vw(iik-1) + w(v(k),vk(iik))
        end do
        anorm = vw(mk)
        vw = vw/anorm
        do l=1,mkk
            aux = unifrnd()
            vwaux = vw - aux
            js = minloc(vwaux,MASK=vwaux.GT.0.0)  ! select neighbor according to weight
            do ll=1,mnew
                if(vk(js(1)) == v(ll)) then   ! check if the neighbor has already been added
                    linkc = linkc + 1         ! node is there, just add a link
                    row(linkc) = v(k)
                    col(linkc) = vk(js(1))
                    a_aux(v(k),vk(js(1))) = 2    ! mark link in the matrix
                    a_aux(vk(js(1)),v(k)) = 2
                    exit
                end if
            end do
            if(ll == mnew+1) then     ! if neighbour is new
                mnew = mnew + 1       ! subnetwork size increases by 1
                v(mnew) = vk(js(1))   ! neighbor is saved in v and becomes part of the subnetwork
                idx(vk(js(1))) = mnew
                linkc = linkc + 1     ! add link between v(k) and the selected neighbor
                row(linkc) = v(k)
                col(linkc) = vk(js(1))
                a_aux(v(k),vk(js(1))) = 2    ! mark link in the matrix
                a_aux(vk(js(1)),v(k)) = 2
            end if
        end do
        DEALLOCATE(vw,vwaux)
    end if
end do


! all nodes of subnetwork are stored in v -> construct adjacency matrix jj
ALLOCATE (jj(mnew,mnew))
jj = 0
do k=1,linkc
    jj(idx(row(k)),idx(col(k))) = 1
    jj(idx(col(k)),idx(row(k))) = 1
end do

ALLOCATE(degsamp(mnew))
degsamp = sum(jj,DIM=1)

DEALLOCATE(degsamp)

! find connected clusters
call clusters(jj,mnew,maxsize,nclusters)

! calculate how many nodes of hidden modules have been found
hidden = 0
k = 0
do l=1,imods
    il = 1
    if( module_status(l) == 0) il = 0
        do ll=1,modsize(l)
            k = k + 1
            if(idx(k) /= 0) hidden = hidden + il
        end do
end do

! save the subnetwork
!CALL SAVE_SUB_NET

  do iw=1,mnew
    do jw=1,mnew
      net_out(iw + (jw-1)*n ) = jj(iw,jw)
    end do
  end do

sampled_nodes(1:n) = idx(1:n)
  do iw=1,n
    do jw=1,n
      sampled_edges(iw + (jw-1)*n) = a_aux(iw,jw)
    end do
  end do
DEALLOCATE (jj)
!CLOSE(27)
!CLOSE(28)


! print results on file
!OPEN(UNIT=10,FILE=out_file,STATUS='unknown')

!write(10,*) '   m  size larg-comp  rel-larg-comp  #-comps  hidden-nodes '
!write(10,*) ' ---------------------------------------------------------------'
!write(10,*)
!write(10,112) m,mnew,maxsize,relsize,nclusters,hidden
!close(10)

DEALLOCATE (row,col)
DEALLOCATE(v,vk,prob_aux,key_prob)
DEALLOCATE(a,a_aux,idx,modsize,module_status)
DEALLOCATE(w)
!110 FORMAT(A4,1x,A4)
!112 FORMAT(3(i10,10x),F10.4,10x,2(i10,10x))

END SUBROUTINE subsampling



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE SAMPLING_CRITERION(icrit)
USE globalvar
IMPLICIT double precision(A-H,O-Z)
double precision anorm, sigma, av, y
double precision, ALLOCATABLE :: prob(:),prob_aux(:),x(:),rhoc(:)
INTEGER :: js(1)
ALLOCATE (prob(n))

call rndstart()


prob = 0.0
IF(icrit <= 2) THEN     ! sampling is Random, Lognormal or Fisher
    IF(icrit > 0) THEN
        np = 10000
        ALLOCATE (x(0:np),rhoc(0:np),prob_aux(0:np))
        IF(icrit == 1)  THEN
            av = 1.0D0
            sigma = 0.2D0
            CALL lognormal(np,av,sigma,x,rhoc)   ! generate log-normal distribution
        ELSE
            y = 0.5D0
            CALL fisherlog(np,y,x,rhoc)          ! generate fisher distribution
        END IF
    ELSE
    END IF

    do im=1,imods
        if(im == 1) then
            ijump = 0
        else
            ijump = ijump + modsize(im-1)   ! move from module to module
        end if

        do i=1,modsize(im)
            if(module_status(im) == 1) then
                prob(1+ijump) = 0.0
            else
                IF(icrit > 0) THEN
                    aux = unifrnd()
                    prob_aux = rhoc - aux
                    js = minloc(prob_aux,MASK=prob_aux.GT.0.0)
                    j = js(1)
                    prob(i+ijump) = x(j)     ! lognormal or fisher
                ELSE
                    prob(i+ijump) = 1.0D0      ! random (uniform)
                END IF
            end if
            if(im == 1 .and. i == 1) then
                key_prob(i+ijump) = prob(i+ijump)
            else
                key_prob(i+ijump) = key_prob(i-1+ijump) + prob(i+ijump)
            end if
        end do
    end do

    ELSE IF(icrit == 3) THEN     ! exponential abundance distribution
        do im=1,imods
            if(im == 1) then
                ijump = 0
            else
                ijump = ijump + modsize(im-1)   ! move from module to module
        end if

        do i=1,modsize(im)
            if(module_status(im) == 1) then
                prob(1+ijump) = 0.0D0
            else
                aux = unifrnd()
                prob(i+ijump) = log(1.0D0/aux)
            end if
            if(im == 1 .and. i == 1) then
                key_prob(i+ijump) = prob(i+ijump)
            else
                key_prob(i+ijump) = key_prob(i-1+ijump) + prob(i+ijump)
            end if
        end do
    end do

    ELSE IF(icrit == 4) THEN                        ! sample according to degree
        prob = sum(a,DIM=1)
        DO im=1,imods
            if(im == 1) then
                ijump = 0
            else
                ijump = ijump + modsize(im-1)   ! move from module to module
            end if

            if(module_status(im) == 1) then
                DO i=1,modsize(im)
                    prob(i+ijump) = 0.0D0
                END DO
            end if
        END DO
        key_prob(1) = prob(1)
        do i=2,n
            key_prob(i) = key_prob(i-1) + prob(i)
        end do

    ELSE IF(icrit == 5) THEN                        ! sample according to module
        do im=1,imods
            if(im == 1) then
                ijump = 0
            else
                ijump = ijump + modsize(im-1)      ! move from module to module
            end if
            if(module_status(im) == 1) then
                prob(1+ijump) = 0.0D0
            else
                aux = unifrnd()
                prob(1+ijump) = log(1.0D0/aux)/dble(modsize(im))
            end if
            if(im == 1) then
                key_prob(1) = prob(1)
            else
                key_prob(1+ijump) = key_prob(ijump) + prob(1+ijump)
            end if
            do i=2,modsize(im)
                prob(i+ijump) = prob(1+ijump)
                key_prob(i+ijump) = key_prob(i-1+ijump) + prob(i+ijump)
            end do
    end do

END IF

anorm = dble(key_prob(n))
key_prob = dble(key_prob/anorm)

call rndend()


! key_prob is a vector with entries between 0 and 1
! and contains the cummulative probability of sampling the  nodes
RETURN
END SUBROUTINE SAMPLING_CRITERION




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! generates a log-normal distribution with mean av and variace sigma
! rho(x) = 1/(sqrt(2*pi)*sigma*x) * exp( -(ln(x)-ln(av))^2/(2*sigma^2) )
SUBROUTINE lognormal(np,av,sigma,x,rhoc)
IMPLICIT double precision(A-H,O-Z)
INTEGER np
double precision  xmax, xstep, aux1, aux2, avlog, av, sigma, anorm
double precision rho(np),rhoc(0:np),x(0:np)
xmax =  av + 5.0D0 * sigma
xstep = xmax/dble(np)
x = 0.0D0
rhoc = 0.0D0
aux1 =  1.0D0/(sigma*sqrt(2.0D0*3.1415926D0))
aux2 =  0.5D0/(sigma**2)
avlog = log(av)

do i=1,np
x(i) = x(i-1) + xstep
rho(i) = (aux1/x(i))*exp(-aux2*( log(x(i)) - avlog )**2)
rhoc(i) = rhoc(i-1) + rho(i)
end do
anorm = rhoc(np)
rhoc = rhoc/anorm

END SUBROUTINE lognormal



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! generates a Fisher log-series distribution with parameter y
!
! Sn = alpha*y^n/n  = number of species with n individuals
! S = sum_n Sn = alpha*ln(1/(1-y)) =  total number of species
! Sn/S = - y^n/(n*ln(1-y)) = probability of picking a species with n individuals
!
! N = sum n*Sn = alpha*y/(1-y) = total number of individuals
! y = (N/alpha)/(1+N/alpha) -> 1-y = 1/(1+N/alpha) -> S = alpha*ln(1+N/alpha)

SUBROUTINE fisherlog(np,y,x,rhoc)
IMPLICIT double precision(A-H,O-Z)
double precision xmax, xstep, aux1, y, anorm
INTEGER np
double precision rho(np),rhoc(0:np),x(0:np)

xmax = 10.0D0
xstep = xmax/dble(np)
x = 0.0
rhoc = 0.0D0
aux1 = -1.0D0/log(1-y)
x(0) = 1.0D0

do i=1,np
x(i) = x(i-1) + xstep
rho(i) = aux1*(y**x(i))/x(i)
rhoc(i) = rhoc(i-1) + rho(i)
end do
anorm = rhoc(np)
rhoc = rhoc/anorm

END SUBROUTINE fisherlog











